%%% File    : video_mpeg.erl
%%% Author  : Tony Rogvall <tony@bix.hemma.se>
%%% Description : MPEG image processing
%%% Created :  5 Mar 2003 by Tony Rogvall <tony@bix.hemma.se>

-module(video_mpeg).

-include("erl_img.hrl").
-include("api.hrl").

magic(_) -> false.

mime_type() -> "video/mpeg".

extensions() -> [".mpg", ".mpeg"].

read_info(_Fd) ->
    {error, bad_magic}.


write_info(_Fd, _IMG) ->
    {error, bad_image}.

read(_Fd,_IMG) ->
    {error, bad_image}.

read(_Fd,_IMG,_PixFun,_PixSt) ->
    {error, bad_image}.

write(_Fd,_IMG) ->
    {error, bad_image}.



