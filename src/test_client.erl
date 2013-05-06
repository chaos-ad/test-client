-module(test_client).
-compile(export_all).

-include("piqi/chat_piqi.hrl").
-include("piqi/gateway_piqi.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

start() ->
    start(?MODULE).

start(App) ->
    start_ok(App, application:start(App, permanent)).

stop() ->
    application:stop(?MODULE).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

start_ok(_, ok) ->
    ok;

start_ok(_, {error, {already_started, _App}}) ->
    ok;

start_ok(App, {error, {not_started, Dep}}) when App =/= Dep ->
    ok = start(Dep),
    start(App);

start_ok(App, {error, Reason}) ->
    erlang:error({app_start_failed, App, Reason}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

start_client(Name) ->
    start_client(Name, []).

start_client(Name, Options) ->
    test_client_sup:start_client(Name, Options).

stop_client(Name) ->
    test_client_sup:stop_client(Name).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

service_enable(UserID, ServiceID) ->
    test_client_conn:service_enable(UserID, ServiceID).

service_disable(UserID, ServiceID) ->
    test_client_conn:service_disable(UserID, ServiceID).

service_request(UserID, ServiceID, Request) ->
    test_client_conn:service_request(UserID, ServiceID, Request).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

chat_say_user(UserID, To, Message) ->
    service_request(UserID, <<"chat">>, {say, #chat_say{to={user, To}, body=Message}}).

chat_say_group(UserID, To, Message) ->
    service_request(UserID, <<"chat">>, {say, #chat_say{to={group, To}, body=Message}}).

chat_join_group(UserID, GroupID) ->
    service_request(UserID, <<"chat">>, {join_group, GroupID}).

chat_leave_group(UserID, GroupID) ->
    service_request(UserID, <<"chat">>, {leave_group, GroupID}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
