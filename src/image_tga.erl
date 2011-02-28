%%% File    : image_tga.erl
%%% Author  : Bob Ippolito <bob@redivi.com>
%%% Description : TGA image processing
%%% Created :  27 Feb 2011 by Bob Ippolito <bob@redivi.com>

%% Not a lot of practical use for TGA files but it's a dead simple format to
%% implement. I decided to implement it to be able to render out images when
%% testing.

-module(image_tga).

-include("erl_img.hrl").

-include("api.hrl").

%% -define(debug, true).
-include("dbg.hrl").

-import(lists, [reverse/1]).


read_magic(<<0,
             0:7, ColorMapType:1,
             ImageType,
             ColorMapStart:16/little-signed-integer,
             ColorMapLength:16/little-signed-integer,
             ColorMapBits,
             XStart:16/little-signed-integer,
             YStart:16/little-signed-integer,
             Width:16/little-signed-integer,
             Height:16/little-signed-integer,
             Depth,
             0:1, 0:1, VFlip:1, HFlip:1, 0:4,
             _/binary>>)
  when (Width > 0) andalso (Height > 0) andalso
       (XStart =:= 0) andalso (YStart =:= 0) andalso
       (Depth =:= 1 orelse Depth =:= 8 orelse
        Depth =:= 16 orelse Depth =:= 24 orelse
        Depth =:= 32) andalso
       %% Only 24-bit and 32-bit uncompressed left-to-right supported
       (HFlip =:= 0) andalso
       (ImageType =:= 2) andalso (ColorMapType =:= 0) andalso
       (ColorMapStart =:= 0) andalso (ColorMapLength =:= 0) andalso
       (ColorMapBits =:= 0) ->
    #erl_image{type=?MODULE,
               width=Width,
               height=Height,
               %% We transcode bgr to rgb when reading/writing
               format=case Depth of
                          24 -> r8g8b8;
                          32 -> r8g8b8a8
                      end,
               order=left_to_right,
               depth=8,
               alignment=case VFlip of
                             0 -> 4;
                             1 -> 1
                         end,
               bytes_pp=Depth div 8}.

magic(B) ->
    try
        _ = read_magic(B),
        true
    catch _:_ ->
            false
    end.

mime_type() -> "image/tga".

extensions() -> [ ".tga" ].

read_info(Fd) ->
    try
        {ok, B} = file:read(Fd, 18),
        {ok, read_magic(B)}
    catch _:_ ->
            {error, bad_magic}
    end.

write_info(Fd, #erl_image{width=Width, height=Height, bytes_pp=BPP}) ->
    file:write(Fd,
               <<0, 0:7, 0:1,
                 2,
                 0:16/little-signed-integer,
                 0:16/little-signed-integer,
                 0,
                 0:16/little-signed-integer,
                 0:16/little-signed-integer,
                 Width:16/little-signed-integer,
                 Height:16/little-signed-integer,
                 (BPP * 8),
                 0:1, 0:1, 1:1, 0:1, 0:4>>).

read(Fd,IMG,RowFun,St0) ->
    file:position(Fd, 18),
    case read_pixels(Fd, IMG, RowFun, St0) of
        {ok, PIX} ->
            {ok, IMG#erl_image{pixmaps=[PIX]}};
        Error ->
            Error
    end.

%% Read all rows
read_pixels(Fd, #erl_image{bytes_pp=BPP, width=Width,
                           height=Height, format=Format},
            RowFun, St0) ->
    RowLength = Width * BPP,
    PIX = #erl_pixmap { width = Width, height = Height,
                        format = Format },
    read_pixels(Fd, PIX, 0, Height, RowLength, RowFun, St0).


read_pixels(_Fd, PIX, NRows, NRows, _BytesPerRow, _RowFun, St) ->
    {ok, PIX#erl_pixmap{pixels = St}};
read_pixels(Fd, PIX, Ri, NRows, BytesPerRow, RowFun, St) ->
    case file:read(Fd, BytesPerRow) of
        {ok,Row} ->
            St1 = RowFun(PIX, Row, Ri, St),
            read_pixels(Fd, PIX, Ri+1, NRows, BytesPerRow, RowFun, St1);
        Error ->
            Error
    end.

rgb_to_bgr(Row) ->
    << <<B, G, R>> || <<R, G, B>> <= Row >>.

rgba_to_bgra(Row) ->
    << <<B, G, R, A>> || <<R, G, B, A>> <= Row >>.

bgr_to_rgb(Row) ->
    << <<R, G, B>> || <<B, G, R>> <= Row >>.

bgra_to_rgba(Row) ->
    << <<R, G, B, A>> || <<B, G, R, A>> <= Row >>.

read(Fd, IMG=#erl_image{bytes_pp=BPP}) ->
    read(Fd, IMG,
         case BPP of
             3 ->
                 fun(_, Row, Ri, St) ->
                         ?dbg("tga: load bgr row ~p\n", [Ri]),
                         [{Ri,bgr_to_rgb(Row)}|St]
                 end;
             4 ->
                 fun(_, Row, Ri, St) ->
                         ?dbg("tga: load bgra row ~p\n", [Ri]),
                         [{Ri,bgra_to_rgba(Row)}|St]
                 end
         end,
         []).


write(Fd, IMG=#erl_image{pixmaps=[PM], bytes_pp=BPP}) ->
    write_info(Fd, IMG),
    RowFun = case BPP of
                 3 -> fun rgb_to_bgr/1;
                 4 -> fun rgba_to_bgra/1
             end,
    write_rows(Fd, RowFun, lists:sort(PM#erl_pixmap.pixels)).

write_rows(_Fd, _RowFun, []) ->
    ok;
write_rows(Fd, RowFun, [{_Num, Row} | Rest]) ->
    ok = file:write(Fd, RowFun(Row)),
    write_rows(Fd, RowFun, Rest).


