%%% File    : image_undef.erl
%%% Author  : Tony Rogvall <tony@bix.hemma.se>
%%% Description : Undefined format catch module
%%% Created :  5 Mar 2003 by Tony Rogvall <tony@bix.hemma.se>

-module(image_undef).

-include_lib("erl_img/include/erl_img.hrl").
-include("api.hrl").

magic(_) -> false.

mime_type() -> "".

extensions() -> [].

    
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


