%%%-------------------------------------------------------------------
%%% Copyright (c) 2009-2010 by Evgeny Khirin.
%%%-------------------------------------------------------------------
%%%-------------------------------------------------------------------
%%% File    : build.erl
%%% Author  : Evgeny Khirin <>
%%% Description :
%%%
%%% Created : 22 Feb 2009 by Evgeny Khirin <>
%%%-------------------------------------------------------------------
-module(build).

%% API
-export([compile/0]).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: compile() -> ok | {error, Reason}
%% Description: Builds all applications.
%%--------------------------------------------------------------------
compile() ->
    build_klisp().

%%====================================================================
%% Internal functions
%%====================================================================
%%--------------------------------------------------------------------
%% Function: module_file(Module) -> Filename
%% Description: Returns file name of module.
%%    Filename = string
%%--------------------------------------------------------------------
module_file(Module) ->
    {_Module, _Binary, Filename} = code:get_object_code(Module),
    Filename.

%%--------------------------------------------------------------------
%% Checks file dependencies. Returns true, if file must be recompiled.
%%--------------------------------------------------------------------
check_dependencies(Target, Depends) ->
    TargetLastModified = filelib:last_modified(Target),
    not lists:all(fun(File) ->
                          FileLastModified = filelib:last_modified(File),
                          case filelib:last_modified(File) of
                              0 ->
                                  false;
                              FileLastModified ->
                                  FileLastModified < TargetLastModified
                          end
                  end,
                  [module_file(?MODULE) | Depends]).

%%---------------------------------------------------------------------------
%% fs_object_exist - checks if there is object with such name in FS
%%---------------------------------------------------------------------------
fs_object_exist(Obj) ->
    case file:read_link_info(Obj) of
        {ok, _} ->
            true;
        {error, enoent} ->
            false;
        _ ->
            case file:read_file_info(Obj) of
                {error, enoent} ->
                    false;
                _ ->
                    true
            end
    end.

%%---------------------------------------------------------------------------
%% mkdir_ensure - builds whole directory hierarchy
%%---------------------------------------------------------------------------
mkdir_ensure(Dir) ->
    filelib:ensure_dir(Dir),
    file:make_dir(Dir),
    ok.

%%--------------------------------------------------------------------
%% Executes OS command. If command returns 0, than its output is discarded
%% and ok returned. If exit status is not equal to 0, than command output
%% is dumped to screen and error is returned.
%%--------------------------------------------------------------------
exec(Cmd, Filter) ->
    Opts = [stream, exit_status, use_stdio, stderr_to_stdout, binary],
    Port = open_port({spawn, lists:flatten(Cmd)}, Opts),
    io:format("~s~n", [Cmd]),
    exec_loop(Port, <<>>, Filter).

exec_loop(Port, Res, Filter) ->
    receive
        {Port, {exit_status, 0}} ->
            ok;
        {Port, {exit_status, Err}} ->
            io:format("~s", [filter_bin_str(Res, Filter, 20)]),
            {error, {exit_status, Err}};
        {Port, {data, Data}} ->
            exec_loop(Port, <<Res/binary, Data/binary>>, Filter)
%%        _ ->
%%            exec_loop(Port, Res, Filter)
    end.

%%--------------------------------------------------------------------
%% Function: filter_bin_str(Bin, DoFilter) -> NewBin
%% Description: Filters binary from unprintable characters.
%%    Mode = debug | release. Default is debug
%%--------------------------------------------------------------------
filter_bin_str(Str, true, MaxLines) ->
    filter_bin_str(Str, <<>>, 0, MaxLines);
filter_bin_str(Str, false, _MaxLines) ->
    Str.

filter_bin_str(<<>>, Res, _CurLines, _MaxLines) ->
    Res;
filter_bin_str(_RestData, Res, MaxLines, MaxLines) ->
    <<Res/binary, "\n...\n\n">>;
filter_bin_str(<<H, T/binary>>, Res, CurLines, MaxLines) when H >= 32, H < 128 ->
    filter_bin_str(T, <<Res/binary, H>>, CurLines, MaxLines);
filter_bin_str(<<10, T/binary>>, Res, CurLines, MaxLines) ->
    filter_bin_str(T, <<Res/binary, 10>>, CurLines + 1, MaxLines);
filter_bin_str(<<_, T/binary>>, Res, CurLines, MaxLines) ->
    filter_bin_str(T, Res, CurLines, MaxLines).

%%--------------------------------------------------------------------
%% Compiles C++ source file, returns true if file was indeed compiled
%%--------------------------------------------------------------------
compile_cpp(File, CustOpts, debug) ->
    BaseTarget = filename:join([filename:dirname(File), "obj", filename:basename(File)]),
    Target = BaseTarget ++ ".debug.o",
    DepFile = BaseTarget ++ ".debug.d",
    Amd64CmdRes = os:cmd("uname -a | grep x86_64"),
    I686_32_CmdRes = os:cmd("uname -a | grep i686"),
    CpuArch =
        if
            Amd64CmdRes /= "" ->
                "-march=x86-64 -mfpmath=387";
            I686_32_CmdRes /= "" ->
                "-march=i686"
        end,
    Compiler =
        case filename:extension(File) of
            ".cpp" -> "g++-6";
            ".c"   -> "gcc-6"
        end,
    case is_c_file_changed(Target, DepFile) of
        true ->
            Opts = "-O0 -ggdb -Wall -Werror -MD -DDEBUG -D_LARGEFILE64_SOURCE -D_GNU_SOURCE " ++
                "-D __STDC_LIMIT_MACROS -D __STDC_CONSTANT_MACROS ",
            Cmd = io_lib:format("~s -c \"~s\" -o \"~s\" ~s ~s ~s",
                                [Compiler, File, Target, Opts, CpuArch, CustOpts]),
            ok = exec(Cmd, true),
            true;
        false ->
            false
    end;
compile_cpp(File, CustOpts, release) ->
    BaseTarget = filename:join([filename:dirname(File), "obj", filename:basename(File)]),
    Target = BaseTarget ++ ".release.o",
    DepFile = BaseTarget ++ ".release.d",
    Amd64CmdRes = os:cmd("uname -a | grep x86_64"),
    I686_32_CmdRes = os:cmd("uname -a | grep i686"),
    CpuArch =
        if
            Amd64CmdRes /= "" ->
                "-march=x86-64";
            I686_32_CmdRes /= "" ->
                "-march=i686"
        end,
    Compiler =
        case filename:extension(File) of
            ".cpp" -> "g++-6";
            ".c"   -> "gcc-6"
        end,
    case is_c_file_changed(Target, DepFile) of
        true ->
            Opts = "-O3 -ggdb -Wall -Werror -MD -DNDEBUG -D_LARGEFILE64_SOURCE -D_GNU_SOURCE " ++
                "-D __STDC_LIMIT_MACROS -D __STDC_CONSTANT_MACROS " ++
                "-fno-strict-aliasing ",
            Cmd = io_lib:format("~s -c \"~s\" -o \"~s\" ~s ~s ~s",
                                [Compiler, File, Target, Opts, CpuArch, CustOpts]),
            ok = exec(Cmd, true),
            true;
        false ->
            false
    end.

%%--------------------------------------------------------------------
%% Returns true if C++ file or some file, on which target file depends,
%% are changed.
%%--------------------------------------------------------------------
is_c_file_changed(Target, DepFile) ->
    case fs_object_exist(Target) andalso fs_object_exist(DepFile) of
        false ->
            true;
        true ->
            check_c_dependencies(Target, DepFile)
    end.

check_c_dependencies(Target, DepFile) ->
    {ok, DepData} = file:read_file(DepFile),
    Depends = parse_make_deps(Target, DepData),
    TargetLastModified = filelib:last_modified(Target),
    DepLastModified = filelib:last_modified(DepFile),
    if
        DepLastModified > TargetLastModified ->
            true;
        true ->
            check_dependencies(Target, Depends)
    end.

%%--------------------------------------------------------------------
%% Parses dependendcies in style of makefile
%%--------------------------------------------------------------------
parse_make_deps(_, <<>>) ->
    [];
parse_make_deps(Target, DepData) when is_list(Target) ->
    parse_make_deps(list_to_binary(Target), DepData);
parse_make_deps(Target, DepData00) ->
    TargetSize = size(Target),
    <<Target:TargetSize/binary, $:, DepData10/binary>> = DepData00,
    parse_make_deps(DepData10, [], []).

parse_make_deps(<<H, T/binary>>, Curr, Depends00) when H =< 32; H =:= $\\ ->
    Depends10 =
        case Curr of
            [] ->
                Depends00;
            _ ->
                [lists:reverse(Curr) | Depends00]
        end,
    parse_make_deps(T, [], Depends10);
parse_make_deps(<<H, T/binary>>, Curr, Depends) ->
    parse_make_deps(T, [H | Curr], Depends);
parse_make_deps(<<>>, Curr, Depends) ->
    case Curr of
        [] ->
            lists:reverse(Depends);
        _ ->
            [lists:reverse(Curr) | Depends]
    end.

%%--------------------------------------------------------------------
%% Builds KDB library
%%--------------------------------------------------------------------
build_c_lib(BaseDir, Name, Sources, CustOpts, Mode) ->
    Target = filename:join([BaseDir, "obj", "lib"]) ++
        Name ++ "." ++ atom_to_list(Mode) ++ ".a",
    %% Compile sources
    lists:foreach(fun(X) ->
                          compile_cpp(X, CustOpts, Mode)
                  end,
                  Sources),
    %% Build objects list
    Objects00 =
        lists:map(fun(X) ->
                          [X, ".", atom_to_list(Mode), ".o"]
                  end,
                  lists:map(fun(X) ->
                                    filename:join([BaseDir, "obj", filename:basename(X)])
                            end,
                            Sources)),
    case check_dependencies(Target, Objects00) of
        true ->
            %% Sleep before linking so file system will not resolve time difference
            timer:sleep(1000),
            file:delete(Target),
            Objects10 =
                lists:map(fun(X) ->
                                  ["\"", X, "\" "]
                          end,
                          Objects00),
            Cmd = io_lib:format("ar -crs \"~s\" ~s", [Target, Objects10]),
            ok = exec(Cmd, true);
        false ->
            ok
    end.

%%--------------------------------------------------------------------
%% Builds C unit tests
%%--------------------------------------------------------------------
build_c_test(BaseDir, LocalLibs00, SysLibs, TestSrc, CustOpts, Mode) ->
    Target =
        filename:join([BaseDir, "tests",
                       filename:basename(TestSrc, filename:extension(TestSrc))]) ++
        "." ++ atom_to_list(Mode),
    %% Compile sources
    compile_cpp(TestSrc, CustOpts, Mode),
    %% Build objects list
    Objects00 =
        lists:map(fun(X) ->
                          [X, ".", atom_to_list(Mode), ".o"]
                  end,
                  lists:map(fun(X) ->
                                    filename:join([BaseDir, "obj", filename:basename(X)])
                            end,
                            [TestSrc])),
    ModeOpts =
        case Mode of
            debug ->
                "-ggdb -O0";
            release ->
                "-ggdb -O3"
%%                "-s -O3"
        end,
    case check_dependencies(Target, LocalLibs00 ++ Objects00) of
        true ->
            %% Sleep before linking so file system will not resolve time difference
            timer:sleep(1000),
            Objects10 =
                lists:map(fun(X) ->
                                  ["\"", X, "\" "]
                          end,
                          [Objects00]),
            LocalLibs10 =
                lists:map(fun(X) ->
                                  ["\"", X, "\" "]
                          end,
                          [LocalLibs00]),
            Cmd = io_lib:format("g++ -o \"~s\" ~s ~s ~s ~s",
                                [Target, ModeOpts, Objects10, LocalLibs10, SysLibs]),
            ok = exec(Cmd, true);
        false ->
            ok
    end.

%%--------------------------------------------------------------------
%% Preprocess DynASM files
%%--------------------------------------------------------------------
compile_dynasm(WorkDir) ->
    {ok, DirList} = file:list_dir(WorkDir),
    %% DynAsm sources
    DynAsmSources =
        lists:map(fun(X) ->
                          filename:join(WorkDir, X)
                  end,
                  lists:filter(fun(X) ->
                                       ".dasc" =:= filename:extension(X)
                               end,
                               DirList)),
    lists:foreach(fun(X) ->
                          OutputName = filename:rootname(X, ".dasc"),
                          case check_dependencies(OutputName, [X]) of
                              true ->
                                  Cmd = io_lib:format("lua ./include/dynasm/dynasm.lua -o \"~s\" -I \"~s\" \"~s\"",
                                                      [OutputName, WorkDir, X]),
                                  ok = exec(Cmd, true);
                              false ->
                                  ok
                          end
                  end,
                  DynAsmSources),
    ok.

%%--------------------------------------------------------------------
%% Builds multiple core test
%%--------------------------------------------------------------------
build_klisp() ->
    build_klisp(debug, "lisp", ["perf_research.c", "klisp.c" ]),
    build_klisp(release, "lisp", ["perf_research.c", "klisp.c" ]),
    ok.

build_klisp(Mode, WorkDir, ExecSources) ->
    compile_dynasm(WorkDir),
    mkdir_ensure(filename:join(WorkDir, "obj")),
    mkdir_ensure(filename:join(WorkDir, "tests")),
    {ok, DirList} = file:list_dir(WorkDir),
    %% Build sources list
    AllSources =
        lists:map(fun(X) ->
                          filename:join(WorkDir, X)
                  end,
                  lists:filter(fun(X) ->
                                       ".cpp" =:= filename:extension(X) orelse
                                           ".c" =:= filename:extension(X)
                               end,
                               DirList)),
    %% Tests sources
    TestSources =
        lists:map(fun(X) ->
                          filename:join(WorkDir, X)
                  end,
                  ExecSources),
    %% other sources
    LibSources = AllSources -- TestSources,
    %% buld library for tests
    MyOpts = "-I" ++ WorkDir ++ " -I./include",
    build_c_lib(WorkDir, "klisp", LibSources, MyOpts, Mode),
    %% build tests
    WorkLib = filename:join([WorkDir, "obj", "libklisp."]) ++ atom_to_list(Mode) ++ ".a",
    Libs = "-lrt -lgmp -lunistring -lcapstone -lpthread",
    lists:foreach(fun(X) ->
                          build_c_test(WorkDir, [WorkLib], Libs, X, MyOpts, Mode)
                  end,
                  TestSources),
    ok.
