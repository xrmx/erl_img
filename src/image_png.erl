%%% File    : image_png.erl
%%% Author  : Tony Rogvall <tony@bix.hemma.se>
%%% Description : PNG Files
%%% Created :  5 Mar 2003 by Tony Rogvall <tony@bix.hemma.se>

-module(image_png).

-include_lib("erl_img.hrl").
-include("api.hrl").

-include("dbg.hrl").

-import(lists, [reverse/1]).
-import(erl_img, [attribute/3, set_attribute/3]).
-export([filter/4]).

-define(MAGIC, 137,$P,$N,$G,$\r,$\n,26,$\n).

-define(IHDR, "IHDR"). %% image header
-define(PLTE, "PLTE"). %% palette
-define(IDAT, "IDAT"). %% image data
-define(IEND, "IEND"). %% image trailer

-define(bKGD, "bKGD"). %% background color
-define(cHRM, "cHRM"). %% primary chromaticites and white point
-define(gAMA, "gAMA"). %% Image gamma
-define(hIST, "hIST"). %% Image histogram
-define(pHYs, "pHYs"). %% Physical pixel dimensions
-define(sBIT, "sBIT"). %% Significant bits
-define(tEXt, "tEXt"). %% Textual data
-define(tIME, "tIME"). %% Image last modification time
-define(tRNS, "tRNS"). %% Transparency
-define(zTXt, "zTXt"). %% Compressed textual data

magic(<<?MAGIC, _/binary>>) -> true;
magic(_) -> false.

mime_type() -> "image/png".

extensions() -> [ ".png" ].

read_info(Fd) ->
    case file:read(Fd, 8) of
        {ok, << ?MAGIC >> } ->
            scan_info(Fd, #erl_image { type = ?MODULE }, true);
        {ok, _} ->
            {error, bad_magic};
        Error ->
            Error
    end.

scan_info(Fd, IMG, First) ->
    case read_chunk_hdr(Fd) of
        {ok, Length, Type} ->
            scan_info(Fd, IMG, First, Type, Length);
        Error ->
            Error
    end.

scan_info(Fd, IMG, true, ?IHDR, Length) ->
    case read_chunk_crc(Fd,Length) of
        {ok,  <<Width:32, Height:32, BitDepth:8,
               ColorType:8, CompressionMethod:8,
               FilterMethod:8, InterlaceMethod:8, _/binary >>} ->
            scan_info(Fd, IMG#erl_image {
                            width = Width,
                            height = Height,
                            depth = BitDepth,
                            format = format(ColorType,BitDepth),
                            order  = left_to_right,
                            attributes =
                            [ {'ColorType', ColorType},
                              {'Compression', CompressionMethod},
                              {'Filter', FilterMethod },
                              {'Interlace', InterlaceMethod }]}, false);
        Error -> Error
    end;
scan_info(Fd, IMG, false, ?tEXt, Length) ->
    case read_chunk_crc(Fd, Length) of
        {ok, Bin} ->
            scan_info(Fd, update_txt(IMG, Bin), false);
        Error -> Error
    end;
scan_info(Fd, IMG, false, ?zTXt, Length) ->
    case read_chunk_crc(Fd, Length) of
        {ok, CBin} ->
            Bin = zlib:uncompress(CBin),
            scan_info(Fd, update_txt(IMG, Bin), false);
        Error -> Error
    end;
scan_info(Fd, IMG, false, ?bKGD, Length) ->
    CT = attribute(IMG, 'ColorType', undefined),
    case read_chunk_crc(Fd, Length) of
        {ok, <<Index:8>>} when CT==3 ->
            scan_info(Fd, set_attribute(IMG, 'Background', Index), false);
        {ok, <<Gray:16>>} when CT==0; CT==4 ->
            scan_info(Fd, set_attribute(IMG, 'Background', Gray), false);
        {ok, <<R:16,G:16,B:16>>} when CT==2; CT==6 ->
            scan_info(Fd, set_attribute(IMG, 'Background', {R,G,B}), false);
        {ok, _Data} ->
            ?dbg("bKGD other=~p\n", [_Data]),
            scan_info(Fd, IMG, false);
        Error -> Error
    end;
scan_info(Fd, IMG, false, ?tIME, Length) ->
    case read_chunk_crc(Fd, Length) of
        {ok, <<Year:16, Mon:8, Day:8, H:8, M:8, S:8>>} ->
            scan_info(Fd, IMG#erl_image { mtime = {{Year,Mon,Day},
                                                   {H,M,S}} }, false);
        {ok, _Data} ->
            ?dbg("tIME other=~p\n", [_Data]),
            scan_info(Fd, IMG, false);
        Error -> Error
    end;
scan_info(Fd, IMG, false, ?pHYs, Length) ->
    case read_chunk_crc(Fd, Length) of
        {ok, <<X:32, Y:32, _Unit:8>>} ->
            scan_info(Fd, set_attribute(IMG,'Physical',{X,Y,meter}),false);
        {ok, _Data} ->
            ?dbg("pHYs other=~p\n", [_Data]),
            scan_info(Fd, IMG, false);
        Error -> Error
    end;
scan_info(_Fd, IMG, false, ?IEND, 0) ->
    {ok, IMG};
scan_info(Fd, IMG, false, _Type, Length) ->
    ?dbg("~s skipped=~p\n", [_Type,Length]),
    skip_chunk(Fd, Length),
    scan_info(Fd, IMG, false).

%% Update txt attributes
update_txt(IMG, Txt) ->
    case txt(binary_to_list(Txt), []) of
        {value,{Key,Value}} ->
            case Key of
                'Comment' ->
                    IMG#erl_image { comment = Value };
                _ ->
                    As = [{Key,Value} | IMG#erl_image.attributes],
                    IMG#erl_image { attributes = As }
            end;
        false ->
            IMG
    end.


%% determine the erl_image format
bytes_per_row(gray1,W) -> W div 8;
bytes_per_row(gray2,W) -> W div 4;
bytes_per_row(gray4,W) -> W div 2;
bytes_per_row(gray8,W) -> W;
bytes_per_row(gray16,W) -> W*2;
bytes_per_row(r8g8b8,W) -> W*3;
bytes_per_row(r16g16b16,W) -> W*6;
bytes_per_row(palette1,W) -> W div 8;
bytes_per_row(palette2,W) -> W div 4;
bytes_per_row(palette4,W) -> W div 2;
bytes_per_row(palette8,W) -> W;
bytes_per_row(gray8a8,W) -> W*2;
bytes_per_row(gray16a16,W) -> W*4;
bytes_per_row(r8g8b8a8,W) -> W*4;
bytes_per_row(r16g16b16a16,W) -> W*8.


bpp(gray1) -> 1;
bpp(gray2) -> 1;
bpp(gray4) -> 1;
bpp(gray8) -> 1;
bpp(gray16) -> 2;
bpp(r8g8b8) -> 3;
bpp(r16g16b16) -> 6;
bpp(palette1) -> 1;
bpp(palette2) -> 1;
bpp(palette4) -> 1;
bpp(palette8) -> 1;
bpp(gray8a8)  -> 2;
bpp(gray16a16) -> 4;
bpp(r8g8b8a8) -> 4;
bpp(r16g16b16a16) -> 8.


format(0, 1)  -> gray1;
format(0, 2)  -> gray2;
format(0, 4)  -> gray4;
format(0, 8)  -> gray8;
format(0, 16) -> gray16;
format(2, 8)  -> r8g8b8;
format(2, 16) -> r16g16b16;
format(3, 1)  -> palette1;
format(3, 2)  -> palette2;
format(3, 4)  -> palette4;
format(3, 8)  -> palette8;
format(4, 8)  -> gray8a8;
format(4, 16) -> gray16a16;
format(6, 8)  -> r8g8b8a8;
format(6, 16) -> r16g16b16a16.

%% process text chunk
txt([0|Value], RKey) ->
    {value, {list_to_atom(reverse(RKey)), Value}};
txt([C|Cs], RKey) ->
    txt(Cs,[C|RKey]);
txt([], _) ->
    false.

%% read palette
plte(<<R,G,B, Data/binary>>) ->
    [{R,G,B} | plte(Data)];
plte(<<>>) -> [].

%% IMPLEMENT This:
write_info(_Fd, _IMG) ->
    ok.


read(Fd, IMG) ->
    read(Fd, IMG,
         fun(_, Row, Ri, St) ->
                 ?dbg("png: load row ~p\n", [Ri]),
                 [{Ri,Row}|St] end,
         []).


read(Fd, IMG, RowFun, St0) ->
    file:position(Fd, 8), %% skip magic
    Z = zlib:open(),
    zlib:inflateInit(Z),
    Resp = read_image(Fd, [], undefined, Z),
    zlib:close(Z),
    case Resp of
        {ok, Binary, Palette} ->
            {ok,Pixmap} = create_pixmap(IMG, Binary, Palette, RowFun, St0),
            {ok, IMG#erl_image { pixmaps = [Pixmap],
                                 palette = Palette }};
        Error -> Error
    end.

create_pixmap(IMG, Bin, Palette, RowFun, St0) ->
    Interlace = attribute(IMG, 'Interlace', 0),
    Pix0 = #erl_pixmap { width  = IMG#erl_image.width,
                         height = IMG#erl_image.height,
                         palette = Palette,
                         format  = IMG#erl_image.format },
    Bpp = bpp(IMG#erl_image.format),
    BytesPerRow = bytes_per_row(
                    IMG#erl_image.format,IMG#erl_image.width),
    case Interlace of
        0 ->
            raw_data(Bin,Pix0,RowFun,St0,0,Bpp,BytesPerRow);
        1 ->
            interlaced_data(Bin, Pix0, RowFun, St0, Bpp)
    end.


raw_data(Bin,Pix,RowFun,St0,Ri,Bpp,Width) ->
    case Bin of
        <<Filter:8,Row:Width/binary,Bin1/binary>> ->
            Prior = case St0 of
                        [] -> <<>>;
                        [{_,Row0}|_] -> Row0
                    end,
            Row1 = filter(Filter,Bpp,Row,Prior), %% Filter method=0 assumed
            St1 = RowFun(Pix,Row1,Ri,St0),
            raw_data(Bin1,Pix,RowFun,St1,Ri+1,Bpp,Width);
        _ ->
            {ok, Pix#erl_pixmap { pixels = St0 }}
    end.

interlaced_data(Bin, Pix0=#erl_pixmap{width=Width, height=Height},
                RowFun, St0, Bpp) ->
    [P1, P2, P3, P4, P5, P6, P7] = interlaced_pass(Bin, Width, Height, Bpp, 1),
    Passes = {P1, P2, P3, P4, P5, P6, P7},
    Cols = lists:seq(0, Width - 1),
    merge_adam7(Passes, Pix0, RowFun, St0, 0, Bpp, Cols, Height).

merge_adam7(_P, Pix, _RowFun, St0, Height, _Bpp, _Cols, Height) ->
    {ok, Pix#erl_pixmap{pixels=St0}};
merge_adam7(P, Pix, RowFun, St0, Ri, Bpp, Cols, Height) ->
    St1 = RowFun(Pix, merge_adam7_row(P, Ri, Bpp, Cols), Ri, St0),
    merge_adam7(P, Pix, RowFun, St1, 1 + Ri, Bpp, Cols, Height).

%% 1 6 4 6 2 6 4 6
%% 7 7 7 7 7 7 7 7
%% 5 6 5 6 5 6 5 6
%% 7 7 7 7 7 7 7 7
%% 3 6 4 6 3 6 4 6
%% 7 7 7 7 7 7 7 7
%% 5 6 5 6 5 6 5 6
%% 7 7 7 7 7 7 7 7
merge_adam7_row(P, Ri, _Bpp, _Cols) when Ri band 1 =:= 1 ->
    %% 7 7 7 7 7 7 7 7
    array:get(Ri, element(7, P));
merge_adam7_row(P, Ri, Bpp, Cols) ->
    << <<(adam7_pixel(P, Ri, Bpp, Col))/binary>> || Col <- Cols >>.

adam7_pixel(P, Ri, Bpp, Col) when Ri band 1 =:= 1 ->
    %% [7 7 7 7 7 7 7 7]
    binary_part(array:get(Ri, element(7, P)), Col * Bpp, Bpp);
adam7_pixel(P, Ri, Bpp, Col) when Ri band 7 =:= 0 andalso Col band 7 =:= 0 ->
    %% [1] 6 4 6 2 6 4 6
    binary_part(array:get(Ri, element(1, P)), (Col div 8) * Bpp, Bpp);
adam7_pixel(P, Ri, Bpp, Col) when Ri band 7 =:= 0 andalso Col band 1 =:= 1 ->
    %% 1 [6] 4 [6] 2 [6] 4 [6]
    binary_part(array:get(Ri, element(6, P)), (Col div 2) * Bpp, Bpp);
adam7_pixel(P, Ri, Bpp, Col) when Ri band 7 =:= 0 andalso
                                  (Col band 7 =:= 2 orelse Col band 7 =:= 6) ->
    %% 1 6 [4] 6 2 6 [4] 6
    binary_part(array:get(Ri, element(4, P)), (Col div 4) * Bpp, Bpp);
adam7_pixel(P, Ri, Bpp, Col) when Ri band 7 =:= 0 andalso
                                  (Col band 7 =:= 2 orelse Col band 7 =:= 4) ->
    %% 1 6 4 6 [2] 6 4 6
    binary_part(array:get(Ri, element(2, P)), (Col div 8) * Bpp, Bpp);
adam7_pixel(P, Ri, Bpp, Col) when Ri band 7 =:= 2 orelse Ri band 7 =:= 6 ->
    %% [5 6 5 6 5 6 5 6]
    binary_part(array:get(Ri, element(5 + (Col band 1), P)),
                (Col div 2) * Bpp, Bpp);
adam7_pixel(P, Ri, Bpp, Col) when Ri band 7 =:= 4 andalso Col band 3 =:= 0 ->
    %% [3] 6 4 6 [3] 6 4 6
    binary_part(array:get(Ri, element(3, P)), (Col div 4) * Bpp, Bpp);
adam7_pixel(P, Ri, Bpp, Col) when Ri band 7 =:= 4 andalso Col band 1 =:= 1 ->
    %% 3 [6] 4 [6] 3 [6] 4 [6]
    binary_part(array:get(Ri, element(6, P)), (Col div 2) * Bpp, Bpp);
adam7_pixel(P, Ri, Bpp, Col) when Ri band 7 =:= 4 andalso
                                  (Col band 7 =:= 2 orelse Col band 7 =:= 6) ->
    %% 3 6 [4] 6 3 6 [4] 6
    binary_part(array:get(Ri, element(4, P)), (Col div 4) * Bpp, Bpp).




interlaced_pass(_Bin, _Width, _Height, _Bpp, 8) ->
    [];
interlaced_pass(Bin, Width, Height, Bpp, N) ->
    {RowLen, RowInc, RowStart} = get_row_info(Bpp, Width, N),
    {Data, Bin1} = read_interlaced_data(Bin, Width, Height, RowStart, Bpp,
                                        RowLen, RowInc, []),
    [array:from_orddict(lists:reverse(Data))
     | interlaced_pass(Bin1, Width, Height, Bpp, 1 + N)].

read_interlaced_data(Bin, _Width, Height, RowNum, _Bpp, RowLen, _RowInc, St0)
  when RowNum >= Height orelse RowLen =:= 0 ->
    {St0, Bin};
read_interlaced_data(Bin, Width, Height, RowNum, Bpp, RowLen, RowInc, St0) ->
    case Bin of
        <<Filter:8,Row:RowLen/binary,Bin1/binary>> ->
            Prior = case St0 of
                        [] -> <<>>;
                        [{_,Row0}|_] -> Row0
                    end,
            Row1 = filter(Filter,Bpp,Row,Prior), %% Filter method=0 assumed
            read_interlaced_data(Bin1, Width, Height, RowInc + RowNum, Bpp,
                                 RowLen, RowInc,
                                 [{RowNum, Row1} | St0]);
        _ ->
            {St0, Bin}
    end.

get_row_info(BPP, Width, Pass) ->
    RowLen = (Width + adam7(offset, Pass)) div adam7(col_increment, Pass),
    {RowLen * BPP, adam7(row_increment, Pass), adam7(starting_row, Pass)}.

adam7(offset, N) -> element(N, { 7, 3, 3, 1, 1, 0, 0 });
%% adam7(starting_col, N) -> element(N, { 0, 4, 0, 2, 0, 1, 0 });
adam7(starting_row, N) -> element(N, { 0, 0, 4, 0, 2, 0, 1 });
adam7(col_increment, N) -> element(N, { 8, 8, 4, 4, 2, 2, 1 });
adam7(row_increment, N) -> element(N, { 8, 8, 8, 4, 4, 2, 2 }).


filter(0,_,Row,_Prior) -> Row;
filter(1, Bpp, Row, Prior) -> filter_sub(Row,Prior,Bpp);
filter(2, Bpp, Row,Prior) -> filter_up(Row,Prior,Bpp);
filter(3, Bpp, Row,Prior) -> filter_avg(Row,Prior,Bpp);
filter(4, Bpp, Row,Prior) -> filter_paeth(Row,Prior,Bpp).

%%
%% Raw(x) = Sub(x) + Raw(x-bpp)  [ Sub(x) = Raw(x) - Raw(x-bpp) ]
%%
filter_sub(Sub,_Prior,Bpp) ->
    Rn = lists:duplicate(Bpp, 0),
    Rm = [],
    filter_sub(Sub, 0, size(Sub), [], Rn, Rm).

filter_sub(_Sub, X, X, Acc, _, _) ->
    list_to_binary(reverse(Acc));
filter_sub(Sub, X, N, Acc, [Rxb|Rn], Rm) ->
    <<_:X/binary, Sx:8, _/binary>> = Sub,
    Rx = (Sx + Rxb) band 16#ff,
    if Rn == [] ->
            filter_sub(Sub,X+1,N,[Rx|Acc],reverse([Rx|Rm]),[]);
       true ->
            filter_sub(Sub,X+1,N,[Rx|Acc],Rn,[Rx|Rm])
    end.
%%
%% Raw(x) = Up(x) + Prior(x) [ Up(x) = Raw(x) - Prior(x) ]
%%
filter_up(Up, Prior, _Bpp) ->
    filter_up(Up, Prior, 0, size(Up), []).

filter_up(_Up, _Prior, X, X, Acc) ->
    list_to_binary(reverse(Acc));
filter_up(Up, Prior, X, N, Acc) ->
    <<_:X/binary,Ux:8,_/binary>> = Up,
    Px = case Prior of
             <<_:X/binary,Pi,_/binary>> -> Pi;
             _ -> 0
         end,
    Rx = (Ux + Px) band 16#ff,
    filter_up(Up,Prior,X+1,N,[Rx|Acc]).

%%
%% Raw(x) = Avarage(x) + floor((Raw(x-bpp)+Prior(x))/2)
%%    [ Avarage(x) = Raw(x) - floor((Raw(x-bpp)+Prior(x))/2) ]
%%

filter_avg(Avg, Prior,Bpp) ->
    Rn = lists:duplicate(Bpp, 0),
    Rm = [],
    filter_avg(Avg, Prior,  0, size(Avg), [], Rn, Rm).

filter_avg(_Avg,_Prior, X, X, Acc, _, _) ->
    list_to_binary(reverse(Acc));
filter_avg(Avg, Prior, X, N, Acc, [Rxb|Rn], Rm) ->
    <<_:X/binary, Ax:8, _/binary>> = Avg,
    Px = case Prior of
             <<_:X/binary,Pi,_/binary>> -> Pi;
             _ -> 0
         end,
    Rx = (Ax + ((Rxb+Px) div 2)) band 16#ff,
    if Rn == [] ->
            filter_avg(Avg,Prior,X+1,N,[Rx|Acc],reverse([Rx|Rm]),[]);
       true ->
            filter_avg(Avg,Prior,X+1,N,[Rx|Acc],Rn,[Rx|Rm])
    end.

%%
%% Paeth(x) = Raw(x) -
%%            PaethPredictor(Raw(x-bpp),Prior(x),Prior(x-bpp))
%%
%% Raw(x) = Paeth(x) + PaethPredictor(Raw(x-bpp),Prior(x),Prior(x-bpp))
%%
filter_paeth(Pae,Prior,Bpp) ->
    Pn = Rn = lists:duplicate(Bpp, 0),
    Pm = Rm = [],
    filter_pae(Pae, Prior, 0, size(Pae), [], Rn, Rm, Pn, Pm).


filter_pae(_Pae, _Prior, X, X, Acc, _Rn, _Rm, _Pn, _Pm) ->
    list_to_binary(reverse(Acc));
filter_pae(Pae, Prior, X, N, Acc, [Rxb|Rn], Rm, [Pxb|Pn], Pm) ->
    <<_:X/binary, PAx:8, _/binary>> = Pae,
    Px = case Prior of
             <<_:X/binary,Pi,_/binary>> -> Pi;
             _ -> 0
         end,
    Rx = (PAx + paethPredictor(Rxb, Px, Pxb)) band 16#ff,
    if Rn == [] ->
            filter_pae(Pae,Prior,X+1,N,[Rx|Acc],
                       reverse([Rx|Rm]),[],
                       reverse([Px|Pm]),[]);
       true ->
            filter_pae(Pae,Prior,X+1,N,[Rx|Acc],
                       Rn,[Rx|Rm],
                       Pn,[Px|Pm])
    end.

-define(dabs(X,Y),
        if (X) > (Y) -> (X) - (Y);
           true -> (Y) - (X)
        end).

paethPredictor(A,B,C) ->
    P = A + B - C,
    PA = ?dabs(P,A),
    PB = ?dabs(P,B),
    PC = ?dabs(P,C),
    if PA =< PB, PA =< PC -> A;
       PB =< PC -> B;
       true -> C
    end.








write(_Fd, _IMG) ->
    ok.

read_image(Fd, Acc, Palette, Z) ->
    case read_chunk_hdr(Fd) of
        {ok, Length, ?IDAT} ->
            case read_chunk_crc(Fd, Length) of
                {ok, CBin} ->
                    Blocks = zlib:inflate(Z, CBin),
                    read_image(Fd, [Blocks|Acc], Palette, Z);
                Error -> Error
            end;
        {ok, _Length, ?IEND} ->
            zlib:inflateEnd(Z),
            {ok, list_to_binary(reverse(Acc)), Palette};

        {ok, Length, ?PLTE} ->
            case read_chunk_crc(Fd, Length) of
                {ok, Chunk} ->
                    read_image(Fd, Acc, plte(Chunk), Z);
                Error ->
                    Error
            end;
        {ok, Length, _} ->
            skip_chunk(Fd, Length),
            read_image(Fd, Acc, Palette, Z)
    end.

%%
%% Given chunk header read chunk and check crc
%%
read_chunk_crc(Fd, Length) ->
    file:position(Fd, {cur,-4}),
    LengthWithType = Length+4,
    case file:read(Fd, LengthWithType+4) of
        {ok,<<TypeChunk:LengthWithType/binary, CRC:32>>} ->
            case valid_crc32(TypeChunk, CRC) of
                true ->
                    <<_:32, Chunk/binary>> = TypeChunk,
                    {ok, Chunk};
                false ->
                    {error, bad_crc}
            end;
        {ok,_} ->
            {error, bad_chunk};
        Error ->
            Error
    end.

%%
%% Read the chunk header
%%

read_chunk_hdr(Fd) ->
    case file:read(Fd, 8) of
        {ok, <<Length:32, Type:4/binary>>} ->
            Tag = binary_to_list(Type),
            ?dbg("chunk: type = ~p, length=~p\n", [Tag,Length]),
            {ok, Length, Tag};
        Error ->
            Error
    end.


skip_chunk(Fd, Length) ->
    file:position(Fd, {cur,Length+4}).

valid_crc32(Binary, CRC32) ->
    Z = zlib:open(),
    Value = zlib:crc32(Z, Binary),
    zlib:close(Z),
    ?dbg("crc check: ~p == ~p\n", [CRC32, Value]),
    CRC32 == Value.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
%%
%% These tests use the pngsuite of test pngs from the libpng
%% website to verify all sorts of things.  The intentionally corrupt
%% pngs live in priv/pngsuite/corrupted because I didn't want
%% to deal with them right now, see should probably write
%% some more acceptance tests that ensures those fail for
%% specific reasons.
%%
png_suite_files() ->
    application:start(erl_img),
    PrivDir = code:priv_dir(erl_img),
    filelib:wildcard(PrivDir ++ "/pngsuite/_skip_*.png").
    %% filelib:wildcard(PrivDir ++ "/pngsuite/*.png").
    %% filelib:wildcard("/Users/dreid/Untitled-*.png").

%% Use a macro here so png_suite_*_test_ shows up in the eunit output.
-define(PNG_SUITE_TEST(TestFun),
        [{filename:basename(FName),
          fun() ->
                  TestFun(FName)
          end}
         || FName <- png_suite_files()]).

png_suite_basic_interlace_test() ->
    application:start(erl_img),
    PrivDir = code:priv_dir(erl_img),
    [F0, F1] = lists:sort(
                 filelib:wildcard(PrivDir ++ "/pngsuite/bas[in]2c08.png")),
    Pm = fun (F) ->
                 {ok, Img=#erl_image{pixmaps=[PM]}} = erl_img:load(F),
                 %% erl_img:save("/tmp/" ++ filename:basename(F) ++ ".tga",
                 %%              Img#erl_image{type=image_tga}),
                 #erl_pixmap{pixels=Rows} = PM,
                 lists:sort(Rows)
         end,
    ?assertEqual(
       Pm(F0),
       Pm(F1)),
    ok.

merge_adam7_row_test_() ->
    %% 1 6 4 6 2 6 4 6
    %% 7 7 7 7 7 7 7 7
    %% 5 6 5 6 5 6 5 6
    %% 7 7 7 7 7 7 7 7
    %% 3 6 4 6 3 6 4 6
    %% 7 7 7 7 7 7 7 7
    %% 5 6 5 6 5 6 5 6
    %% 7 7 7 7 7 7 7 7
    P = {array:from_orddict([{0, <<1>>}]),
         array:from_orddict([{0, <<2>>}]),
         array:from_orddict([{4, <<3, 3>>}]),
         array:from_orddict([{0, <<4, 4>>},
                             {4, <<4, 4>>}]),
         array:from_orddict([{2, <<5, 5, 5, 5>>},
                             {6, <<5, 5, 5, 5>>}]),
         array:from_orddict([{0, <<6, 6, 6, 6>>},
                             {2, <<6, 6, 6, 6>>},
                             {4, <<6, 6, 6, 6>>},
                             {6, <<6, 6, 6, 6>>}]),
         array:from_orddict([{1, <<7, 7, 7, 7, 7, 7, 7, 7>>},
                             {3, <<7, 7, 7, 7, 7, 7, 7, 7>>},
                             {5, <<7, 7, 7, 7, 7, 7, 7, 7>>},
                             {7, <<7, 7, 7, 7, 7, 7, 7, 7>>}])},
    ExpectList = [{0, <<1, 6, 4, 6, 2, 6, 4, 6>>},
                  {1, <<7, 7, 7, 7, 7, 7, 7, 7>>},
                  {2, <<5, 6, 5, 6, 5, 6, 5, 6>>},
                  {3, <<7, 7, 7, 7, 7, 7, 7, 7>>},
                  {4, <<3, 6, 4, 6, 3, 6, 4, 6>>},
                  {5, <<7, 7, 7, 7, 7, 7, 7, 7>>},
                  {6, <<5, 6, 5, 6, 5, 6, 5, 6>>},
                  {7, <<7, 7, 7, 7, 7, 7, 7, 7>>}],
    [(fun ({N, Expect}) ->
             fun () ->
                     ?assertEqual(
                        Expect,
                        merge_adam7_row(P, N, 1, lists:seq(0, 7)))
             end
     end)(X) || X <- ExpectList].


png_suite_read_test_() ->
    ?PNG_SUITE_TEST(
      fun(FileName) ->
              {ok, Fd} = file:open(FileName, [read, binary]),
              {ok, Info} = read_info(Fd),
              {ok, 0} = file:position(Fd, 0),
              ?assertMatch({ok, _}, read(Fd, Info))
      end).

png_suite_magic_test_() ->
    ?PNG_SUITE_TEST(
      fun(FileName) ->
              {ok, Bin} = file:read_file(FileName),
              ?assertEqual(true, magic(Bin))
      end).

non_png_magic_test() ->
    ?assertEqual(false, magic(<<"notapng">>)).

mime_type_test() ->
    ?assertEqual("image/png", mime_type()).

extensions_test() ->
    ?assertEqual([".png"], extensions()).

-endif.
