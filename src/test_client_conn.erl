-module(test_client_conn).
-behaviour(gen_fsm).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% gen_fsm callbacks:
-export([init/1, handle_info/3, handle_event/3, handle_sync_event/4, terminate/3, code_change/4]).

-compile(export_all).

-include("piqi/chat_piqi.hrl").
-include("piqi/gateway_piqi.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-record(state, {socket, userid, options}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

start_link(UserID, Options) ->
    gen_fsm:start_link({global, {?MODULE, UserID}}, ?MODULE, [UserID, Options], []).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

service_enable(UserID, ServiceID) ->
    gen_fsm:sync_send_event({global, {?MODULE, UserID}}, {service_enable, ServiceID}).

service_disable(UserID, ServiceID) ->
    gen_fsm:sync_send_event({global, {?MODULE, UserID}}, {service_disable, ServiceID}).

service_request(UserID, ServiceID, Request) ->
    gen_fsm:sync_send_event({global, {?MODULE, UserID}}, {service_request, ServiceID, Request}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init([UserID, Options]) ->
    gen_fsm:send_event_after(0, connect),
    {ok, disconnected, #state{
        userid   = UserID,
        options  = Options
    }}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

disconnected(connect, State=#state{userid=UserID, options=Options}) ->
    Port = get_env(port, Options),
    Host = get_env(host, Options),
    lager:debug("['~s']: connecting to the gateway...", [UserID]),
    case gen_tcp:connect(Host, Port, [binary, {packet, 4}, {active, true}]) of
        {ok, Socket} ->
            lager:debug("['~s']: connecting to the gateway: ok", [UserID]),
            gen_fsm:send_event_after(0, login),
            {next_state, connected, State#state{socket=Socket}};
        {error, Error} ->
            lager:error("['~s']: connecting to the gateway: ~p", [UserID, Error]),
            gen_fsm:send_event_after(5000, connect),
            {next_state, disconnected, State}
    end.

disconnected({service_enable, _}, _, State) ->
    {reply, {error, disconnected}, disconnected, State};
disconnected({service_disable, _}, _, State) ->
    {reply, {error, disconnected}, disconnected, State};
disconnected({service_request, _, _}, _, State) ->
    {reply, {error, disconnected}, disconnected, State}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

connected(login, State=#state{userid=UserID, socket=Socket}) ->
    Passwd = get_env(pass, State#state.options),
    lager:debug("['~s']: authenticating...", [UserID]),
    GateRequest = #gateway_login{userid=UserID, passwd=Passwd},
    case make_request({login, GateRequest}, Socket) of
        ok ->
            lager:debug("['~s']: authenticating: ok", [UserID]),
            {next_state, idle, State};
        {error, Error} ->
            lager:error("['~s']: authenticating: ~p", [UserID, Error]),
            gen_fsm:send_event_after(5000, login),
            {next_state, connected, State}
    end.

connected({service_enable, _}, _, State) ->
    {reply, {error, unauthorized}, disconnected, State};
connected({service_disable, _}, _, State) ->
    {reply, {error, unauthorized}, disconnected, State};
connected({service_request, _, _}, _, State) ->
    {reply, {error, unauthorized}, disconnected, State}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

idle({gateway_event, {service_disabled, ServiceID}}, State=#state{userid=UserID}) ->
    lager:debug("['~s']: service '~s' disabled", [UserID, ServiceID]),
    {next_state, idle, State};

idle({service_event, #gateway_service_event{service=ServiceID, payload=Payload}}, State=#state{userid=UserID}) ->
    try decode_service_event(ServiceID, Payload) of
        {message, #chat_message{from=From, to={user, To}, body=Body}} ->
            lager:debug("['~s']: received message from user '~s': ~p", [To, From, Body]);
        {message, #chat_message{from=From, to={group, To}, body=Body}} ->
            lager:debug("['~s']: received message from user '~s@~s': ~p", [UserID, From, To, Body]);
        {user_left_group, #chat_user_left_group{userid=Who, groupid=GroupID}} ->
            lager:debug("['~s']: user '~s' left the group '~s'", [UserID, Who, GroupID]);
        {user_joined_group, #chat_user_joined_group{userid=Who, groupid=GroupID}} ->
            lager:debug("['~s']: user '~s' joined the group '~s'", [UserID, Who, GroupID]);
        ParsedEvent ->
            lager:debug("['~s']: received event from service '~s': ~p", [UserID, ServiceID, ParsedEvent])
    catch
        _:decoding_failed ->
            lager:error("['~s']: received unknown event from service '~s': ~p", [UserID, ServiceID, Payload])
    end,
    {next_state, idle, State}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

idle({service_enable, ServiceID}, _, State=#state{socket=Socket}) ->
    GateRequest = #gateway_service_enable{service=ServiceID},
    case make_request({service_enable, GateRequest}, Socket) of
        ok -> 
            {reply, ok, idle, State};
        {error, Error} -> 
            {reply, {error, Error}, idle, State}
    end;

idle({service_disable, ServiceID}, _, State=#state{socket=Socket}) ->
    GateRequest = #gateway_service_disable{service=ServiceID},
    case make_request({service_disable, GateRequest}, Socket) of
        ok -> 
            {reply, ok, idle, State};
        {error, Error} -> 
            {reply, {error, Error}, idle, State}
    end;

idle({service_request, ServiceID, Request}, _, State=#state{socket=Socket}) ->
    RawRequest = encode_service_request(ServiceID, Request),
    GateRequest = #gateway_service_request{service=ServiceID, payload=RawRequest},
    case make_request({service_request, GateRequest}, Socket) of
        ok ->
            {reply, ok, idle, State};
        {error, Error} -> 
            {reply, {error, Error}, idle, State};
        {service_response, RawResponse} ->
            {reply, decode_service_response(ServiceID, RawResponse), idle, State}
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

handle_event(Event, StateName, State=#state{userid=UserID}) ->
    lager:warning("['~s']: unhandled event for all states: ~p", [UserID, Event]),
    {next_state, StateName, State}.

handle_sync_event(Event, _, StateName, State=#state{userid=UserID}) ->
    lager:warning("['~s']: unhandled sync event for all states: ~p", [UserID, Event]),
    {next_state, StateName, State}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

handle_info({tcp, Socket, Data}, StateName, State=#state{socket=Socket}) ->
    ?MODULE:StateName(decode_packet(Data), State);

handle_info({tcp_closed, Socket}, _, State=#state{userid=UserID, socket=Socket}) ->
    lager:debug("['~s']: disconnected from the gateway", [UserID]),
    gen_fsm:send_event_after(5000, connect),
    {next_state, disconnected, State#state{socket=undefined}};

handle_info({tcp_error, Socket, Reason}, _, State=#state{userid=UserID, socket=Socket}) ->
    lager:debug("['~s']: disconnected from the gateway: ~p", [UserID, Reason]),
    gen_fsm:send_event_after(5000, connect),
    {next_state, disconnected, State#state{socket=undefined}};

handle_info(Info, StateName, State=#state{userid=UserID}) ->
    lager:warning("['~s']: unhandled info: ~p", [UserID, Info]),
    {next_state, StateName, State}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

terminate(Reason, _, #state{userid=UserID}) ->
    lager:debug("['~s']: terminated: ~p", [UserID, Reason]).

code_change(_, StateName, State, _) ->
    {ok, StateName, State}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Internal functions:

decode_packet(Binary) ->
    try
        gateway_piqi:parse_gateway2client(Binary)
    catch
        _:_ -> throw(decoding_failed)
    end.

encode_packet(Packet) ->
    try
        gateway_piqi:gen_client2gateway(Packet)
    catch
        _:_ -> throw(encoding_failed)
    end.

encode_service_request(<<"chat">>, Request) ->
    try
        iolist_to_binary(chat_piqi:gen_client2chat(Request))
    catch
        _:_ -> throw(encoding_failed)
    end;
encode_service_request(_, Request) -> Request.

decode_service_response(<<"chat">>, RawResponse) ->
    try
        chat_piqi:parse_chat2client(RawResponse)
    catch
        _:_ -> throw(decoding_failed)
    end;
decode_service_response(_, RawResponse) -> RawResponse.

decode_service_event(<<"chat">>, RawEvent) ->
    try
        chat_piqi:parse_event(RawEvent)
    catch
        _:_ -> throw(decoding_failed)
    end;
decode_service_event(_, RawEvent) -> RawEvent.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

make_request(Request, Socket) ->
    try
        ok = inet:setopts(Socket, [{active, false}]),
        ok = gen_tcp:send(Socket, encode_packet(Request)),
        receive_response(Socket)
    after
        ok = inet:setopts(Socket, [{active, true}])
    end.

receive_response(Socket) ->
    case gen_tcp:recv(Socket, 0) of
        {error, Error} -> {error, Error};
        {ok, Response} ->
            case decode_packet(Response) of
                {service_event, Event} ->
                    gen_fsm:send_event(self(), {service_event, Event}),
                    receive_response(Socket);
                {gateway_event, Event} ->
                    gen_fsm:send_event(self(), {gateway_event, Event}),
                    receive_response(Socket);
                Other ->
                    Other
            end
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

get_env(Name, Options) ->
    case proplists:get_value(Name, Options) of
        undefined ->
            DefName = list_to_atom("default_" ++ atom_to_list(Name)),
            case application:get_env(DefName) of
                undefined -> throw({missing_argument, Name});
                {ok, Value} -> Value
            end;
        Value -> Value
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
