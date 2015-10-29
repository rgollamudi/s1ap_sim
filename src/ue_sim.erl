%% @author rgollamudi
%% @doc @todo Add description to ue_sim.


-module(ue_sim).
-behaviour(gen_fsm).
-export([init/1,  state_name/3, handle_event/3, handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).


-include("s1ap.hrl").
-include("nas.hrl").
-include("ueSim.hrl").


%% ====================================================================
%% API functions
%% ====================================================================
-export([start_link/1, starting/2, deregistered/2, registered/2, mk_esm/2]).



%% ====================================================================
%% Behavioural functions
%% ====================================================================
-record(state, {connect_to_name,
				connect_to_pid,
				enb_sock,
				enb_snd_fn,				%% holds the enb send function
				ueS1apId,
				ueTblRef,
				enbTblRef,
				ueConfig,
				secInfo = #ueSecInfo{sharedKey = 0}
				, timeout = 10000
			   , upLinkRate = 128000
			   , dlLinkRate = 1000000
			   , attachParams = []
  				, t3412Ref
  				, defaultT3412Timer = 10000		%% 10 sec
				, nasKsi = <<0:4>>
				, nasCipher = ?EEA0
				, nasIntegrity = ?EIA1
			   }).

-record(cxtParams, {  upLinkRate = 0
					, dlLinkRate = 0
					, erabId
					, arpPrio
					, arpVul
					, gwIpAddr
					, gtpTid
					, attachResult
					, t3412
					, taiList
					, epsBearerId
					, pti
					, epsQos
					, apnName
					, pdnAddr
					, guti
					, lai
					, msId
					, t3402
					, t3423
					, eplms
					, epsNtwk
					, addResult
					, t3412Ext
					, t3324
					, esmTransId
					, negotiatedQos
					, negotiatedLlcSapi
					, radioPrio
					, pktFlowId
					, pco
					, apnAmbr
					, esmCause
					}).



start_link(StartParams) ->
	gen_fsm:start_link(?MODULE, [StartParams], []).



%% init/1
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_fsm.html#Module:init-1">gen_fsm:init/1</a>
-spec init(Args :: term()) -> Result when
	Result :: {ok, StateName, StateData}
			| {ok, StateName, StateData, Timeout}
			| {ok, StateName, StateData, hibernate}
			| {stop, Reason}
			| ignore,
	StateName :: atom(),
	StateData :: term(),
	Timeout :: non_neg_integer() | infinity,
	Reason :: term().
%% ====================================================================
init([StartParams]) ->
	%%io:format("in Ue_sim   Process: ~w, Imsi: ~w~n", [self(), get_param(StartParams#ueParams.ueConfig, mobileId)]),
	ets:insert(StartParams#ueParams.ueTblRef, 
			   		{StartParams#ueParams.ueS1apId, self(), ?MODULE, #ueInfo{initialUeMsgParams = StartParams#ueParams.ueConfig}}),
	[{_, EnbS1Pid, EnbS1_sock, EnbSndFun} = EnbS1] = ets:lookup(StartParams#ueParams.enbTblRef, StartParams#ueParams.connect_to),

	gen_server:cast(EnbS1Pid, {enb_ready, StartParams#ueParams.ueS1apId, self()}),

    {ok, starting, #state{connect_to_name = StartParams#ueParams.connect_to,
						  connect_to_pid = EnbS1Pid,
						  enb_sock = EnbS1_sock,
						  enb_snd_fn = EnbSndFun,
					  	  ueS1apId = StartParams#ueParams.ueS1apId,
						  ueTblRef = StartParams#ueParams.ueTblRef,
						  enbTblRef = StartParams#ueParams.enbTblRef,
						  ueConfig = StartParams#ueParams.ueConfig}, 1000}.



%% starting/2
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_fsm.html#Module:StateName-2">gen_fsm:StateName/2</a>
-spec starting(Event :: timeout | term(), StateData :: term()) -> Result when
	Result :: {next_state, NextStateName, NewStateData}
			| {next_state, NextStateName, NewStateData, Timeout}
			| {next_state, NextStateName, NewStateData, hibernate}
			| {stop, Reason, NewStateData},
	NextStateName :: atom(),
	NewStateData :: term(),
	Timeout :: non_neg_integer() | infinity,
	Reason :: term().
%% ====================================================================
% @todo implement actual state
starting(enb_ready, StateData) ->
	%%io:format("Ue Sim Process: ~w, Sending InitialUemsg Isim ~w~n, ", [self(), get_param(StateData#state.ueConfig, mobileId)]), 
	EnbSock = ets:lookup_element(StateData#state.enbTblRef, StateData#state.connect_to_name, 3),
	{ok, S1Msg} = s1codec:encodeS1Msg(initiatingMessage, 'initialUEMessage', ?'id-initialUEMessage', ignore,
									  [{?'id-eNB-UE-S1AP-ID', reject, mandatory, StateData#state.ueS1apId},
										{?'id-NAS-PDU', reject, mandatory,
										 	 mk_nas_pdu(StateData, 
														[<<?ATTACH_REQUEST:8>>, 
														 get_param(StateData#state.ueConfig, epsAttachNasKey),
														 (s1_hlpr:ie(mobileId, get_param(StateData#state.ueConfig, mobileId))),
														 (s1_hlpr:ie(ueNetCapability, get_param(StateData#state.ueConfig, ueNetCapability))),
														 mk_esm(get_param(StateData#state.ueConfig, bearerId),
																  [(get_param(StateData#state.ueConfig, pti)),
																   <<?PDN_CONNECTIVITY_REQ:8>>,
																   (get_param(StateData#state.ueConfig, pdnType)),
																   (get_param(StateData#state.ueConfig, reqType)),
																   (get_param(StateData#state.ueConfig, esmInfoTrfFlag)),
																   (get_param(StateData#state.ueConfig, apn)),
																   (get_param(StateData#state.ueConfig, pco)),
																   (get_param(StateData#state.ueConfig, devProperties))
																  ])
													   ]
													  )},
		    							{?'id-TAI', reject, mandatory,  s1_hlpr:s1ap_ie_val(tai, StateData#state.ueConfig)},
		   								{?'id-EUTRAN-CGI', ignore, mandatory, s1_hlpr:s1ap_ie_val(cgi, StateData#state.ueConfig)},
		   								{?'id-RRC-Establishment-Cause', ignore, mandatory, s1_hlpr:s1ap_ie_val(rrcEstCause, StateData#state.ueConfig)},
		   								{?'id-S-TMSI', reject, optional, s1_hlpr:s1ap_ie_val(stmsi, StateData#state.ueConfig)},
		   								{?'id-CSG-Id', reject, optional, s1_hlpr:s1ap_ie_val(csgId, StateData#state.ueConfig)},
		   								{?'id-GUMMEI-ID', reject,  optional, s1_hlpr:s1ap_ie_val(gummeiId, StateData#state.ueConfig)},
		   								{?'id-CellAccessMode', reject, optional, s1_hlpr:s1ap_ie_val(cellAccessMode, StateData#state.ueConfig)},
		   								{?'id-GW-TransportLayerAddress', ignore, optional, s1_hlpr:s1ap_ie_val(gwTlAddr, StateData#state.ueConfig)},
		   								{?'id-RelayNode-Indicator', reject, optional, s1_hlpr:s1ap_ie_val(relayNodeInd, StateData#state.ueConfig)},
		   								{?'id-GUMMEIType', ignore, optional, s1_hlpr:s1ap_ie_val(gummeiType, StateData#state.ueConfig)},
		   								{?'id-Tunnel-Information-for-BBF', ignore, optional, s1_hlpr:s1ap_ie_val(tunInfoBbf, StateData#state.ueConfig)},
		   								{?'id-SIPTO-L-GW-TransportLayerAddress', ignore, optional, s1_hlpr:s1ap_ie_val(siptoTlAddr, StateData#state.ueConfig)},
		   								{?'id-LHN-ID', ignore, optional, s1_hlpr:s1ap_ie_val(lhnId, StateData#state.ueConfig)}
		   							   ]
									 ),
	case (StateData#state.enb_snd_fn)(S1Msg) of
		ok ->  
			%%logger:log_msg("Sent Initial for Isim ~w UE Msg ~w~n", [get_param(StateData#state.ueConfig, mobileId), S1Msg]),
			ok;
		{error, Reason} ->
				io:format("S1 Msg send failed ~w~n", [Reason]),
				error(Reason)
	end,
    {next_state, deregistered, StateData#state{enb_sock = EnbSock}, StateData#state.timeout};

starting(?S1NOTSETUP, StateData) ->
	logger:log_msg("EnbS1 not yet ready ~w~n", [StateData#state.ueS1apId]),
	timer:sleep(1000),
	gen_server:cast(StateData#state.connect_to_pid, {enb_ready, StateData#state.ueS1apId, self()}),
	{next_state, starting, StateData, 1000};

starting(timeout, StateData) ->
	logger:log_msg("State = ~w, stuck in starting state ~w~n", [starting, StateData]),
	{next_state, starting, StateData, hibernate};

starting(Msg, StateData) ->
	logger:log_msg("State = ~w, Process: ~w~nUnknown Msg ~w~n State: ~w~n", [starting, self(), Msg, StateData]),
	{next_state, starting, StateData, hibernate}.



%% deregistered/2
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_fsm.html#Module:StateName-2">gen_fsm:StateName/2</a>
-spec deregistered(Event :: timeout | term(), StateData :: term()) -> Result when
	Result :: {next_state, NextStateName, NewStateData}
			| {next_state, NextStateName, NewStateData, Timeout}
			| {next_state, NextStateName, NewStateData, hibernate}
			| {stop, Reason, NewStateData},
	NextStateName :: atom(),
	NewStateData :: term(),
	Timeout :: non_neg_integer() | infinity,
	Reason :: term().
%% ====================================================================

deregistered(timeout, StateData) ->
	logger:log_msg("Process ~w: No response from MME to Initial UE message ~w~n", [self(), get_param(StateData#state.ueConfig, mobileId)]),
	{next_state, deregistered, StateData};


deregistered({?'id-downlinkNASTransport', {_, {_MsgType, ?'id-downlinkNASTransport', _Criticality, {_S1MsgRecType, IeList} }} = Msg}, 
			 			StateData) ->
	case lists:keyfind(?'id-NAS-PDU', 2, IeList) of
		{_, _, _, NasPdu} -> 
			process_nas_pdu(NasPdu, StateData);
		false -> 
			 logger:log_msg("DownlinkNASTransport msg with no NasPdu rcvd~w~n", [Msg]), 
			 {next_state, deregistered, StateData}
	end;

deregistered({?'id-InitialContextSetup', {_, {_MsgType, ?'id-InitialContextSetup', _Criticality, {_S1MsgRecType, IeList} }}  = Msg}, 
			 			StateData) ->

	
	case validate_initial_context_setup(StateData, IeList) of
		{ok, NewCtxtParams} ->
			CtxtParams = update_attachParams(StateData#state.attachParams, NewCtxtParams),
			send_ueCapabilityIndication(StateData#state{attachParams = CtxtParams}), timer:sleep(1),
			send_initial_context_response(StateData#state{attachParams = CtxtParams}),timer:sleep(1), 
			S1Msg = s1codec:encS1Msg( [ { ?'id-MME-UE-S1AP-ID', reject, mandatory, 
										  ets:lookup_element(StateData#state.ueTblRef, StateData#state.ueS1apId, 3)},
										{?'id-eNB-UE-S1AP-ID' , reject, mandatory, StateData#state.ueS1apId},
							 			{?'id-NAS-PDU', reject, mandatory, 
										   mk_secure_nas_pdu(StateData#state.secInfo, 
											  [<<?PLAIN_NAS:4>>, <<?PD_MM:4>>,
											   <<?ATTACH_COMPLETE:8>>,
											   (mk_esm(<<(get_param(CtxtParams, ?EPS_BEARER_ID)):4>>,
													  [<<(get_param(CtxtParams, ?PTI)):8>>,
														<<?ACTIVATE_DEFAULT_BEARER_ACCP:8>>]))
																					%%mk_esm(?ACTIVATE_DEFAULT_BEARER_ACCP, StateData#state{attachParams = CtxtParams})
											  ] ) },
										{?'id-EUTRAN-CGI', ignore, mandatory, s1_hlpr:s1ap_ie_val(cgi, StateData#state.ueConfig)}, 
							  			{?'id-TAI', ignore, mandatory, s1_hlpr:s1ap_ie_val(tai, StateData#state.ueConfig)}
									  ],
									  initiatingMessage, 'uplinkNASTransport', ?'id-uplinkNASTransport', ignore),
			case (StateData#state.enb_snd_fn)(S1Msg) of
					ok -> %%logger:log_msg("Sent AttComp for Imsi: ~w~n", [get_param(StateData#state.ueConfig, mobileId)]),
						  ok;
					{error, Reason} ->
						io:format("S1 Msg send failed ~w~n", [Reason]),
						error(Reason)
			end,
			T3412Ref = gen_fsm:start_timer(t3412Timer(StateData#state{attachParams = CtxtParams}), ?T3412),
			{next_state, registered, StateData#state{attachParams = CtxtParams, t3412Ref = T3412Ref}};
		{notok, CtxtParams} ->
			send_initial_context_failure(StateData, CtxtParams),
			{next_state, deregistered, StateData}
	end;
	


deregistered(Msg, StateData) ->
	logger:log_msg("Ue Sim ~w got unknown msg in deregistered state ~w~n", [self(), Msg]),
	{next_state, deregistered, StateData}.






%% registered/2
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_fsm.html#Module:StateName-2">gen_fsm:StateName/2</a>
-spec registered(Event :: timeout | term(), StateData :: term()) -> Result when
	Result :: {next_state, NextStateName, NewStateData}
			| {next_state, NextStateName, NewStateData, Timeout}
			| {next_state, NextStateName, NewStateData, hibernate}
			| {stop, Reason, NewStateData},
	NextStateName :: atom(),
	NewStateData :: term(),
	Timeout :: non_neg_integer() | infinity,
	Reason :: term().
%% ====================================================================

registered({timeout, Ref,  ?T3412}, StateData) ->
	S1Msg = s1codec:encS1Msg( [ { ?'id-MME-UE-S1AP-ID', reject, mandatory, 
								  						ets:lookup_element(StateData#state.ueTblRef, StateData#state.ueS1apId, 3)},
							  	{?'id-eNB-UE-S1AP-ID' , reject, mandatory, StateData#state.ueS1apId},
							  	{?'id-NAS-PDU', reject, mandatory, 
													 mk_secure_nas_pdu(StateData#state.secInfo, 
																	[	<<?PLAIN_NAS:4>>, <<?PD_MM:4>>,
															   			<<?TRACKING_AREA_UPDATE_REQUEST:8>>,
																		<<0:4>>, <<(StateData#state.nasKsi):4>>,
																		guti(StateData)			
																		] ) },
								{?'id-EUTRAN-CGI', ignore, mandatory, s1_hlpr:s1ap_ie_val(cgi, StateData#state.ueConfig)}, 
							  	{?'id-TAI', ignore, mandatory, s1_hlpr:s1ap_ie_val(tai, StateData#state.ueConfig)}
							  ],
							  initiatingMessage, 'uplinkNASTransport', ?'id-uplinkNASTransport', ignore),
	case (StateData#state.enb_snd_fn)(S1Msg) of
		ok -> ok; %%logger:log_msg("Sent Tracking Area Update Req ~w~n", [S1Msg]);
		{error, Reason} ->
				io:format("S1 Msg send failed ~w~n", [Reason]),
				error(Reason)
	end,
	T3412Ref = gen_fsm:start_timer(t3412Timer(StateData), ?T3412),
	{next_state, registered, StateData#state{t3412Ref = T3412Ref}};




registered({?'id-downlinkNASTransport', {_, {_MsgType, ?'id-downlinkNASTransport', _Criticality, {_S1MsgRecType, IeList} }}  = Msg}, 
			 			StateData) ->
	case lists:keyfind(?'id-NAS-PDU', 2, IeList) of
		{_, _, _, NasPdu} -> 
			process_nas_pdu(NasPdu, StateData);
		false -> 
			 logger:log_msg("DownlinkNASTransport msg with no NasPdu rcvd~w~n", [Msg])
	end,
	{next_state, registered, StateData};



registered({?'id-UEContextRelease', {_, {_MsgType, ?'id-UEContextRelease' , _Criticality, {_S1MsgRecType, _IeList} }} = Msg},
					StateData) ->
	gen_fsm:cancel_timer(StateData#state.t3412Ref),
	S1Msg = s1codec:encS1Msg( [ { ?'id-MME-UE-S1AP-ID', reject, mandatory, 
								  						ets:lookup_element(StateData#state.ueTblRef, StateData#state.ueS1apId, 3)},
							  	{?'id-eNB-UE-S1AP-ID' , reject, mandatory, StateData#state.ueS1apId}
							  ],
							  successfulOutcome, 'UEContextReleaseComplete', ?'id-UEContextRelease', reject),
	case (StateData#state.enb_snd_fn)(S1Msg) of
		ok -> ok; %%logger:log_msg("Sent Context Release Complete ~w~n", [S1Msg]);
		{error, Reason} ->
				io:format("S1 Msg send failed ~w~n", [Reason]),
				error(Reason)
	end,
	{next_state, starting, StateData#state{t3412Ref = undefined}};


registered({_, {_, ?'id-UERadioCapabilityMatch' , _, {_S1MsgRecType, _IeList} } }  = Msg,  StateData) ->
	%%logger:log_msg("Got UE Radio Capability Match Req ~w~n", [Msg]),
	gen_fsm:cancel_timer(StateData#state.t3412Ref),
	S1Msg = s1codec:encS1Msg( [ { ?'id-MME-UE-S1AP-ID', reject, mandatory, 
								  						ets:lookup_element(StateData#state.ueTblRef, StateData#state.ueS1apId, 3)},
							  	{?'id-eNB-UE-S1AP-ID' , reject, mandatory, StateData#state.ueS1apId},
								{?'id-VoiceSupportMatchIndicator', reject, mandatory, supported}
							  ],
							  successfulOutcome, 'UERadioCapabilityMatchResponse', ?'id-UERadioCapabilityMatch', reject),
	case (StateData#state.enb_snd_fn)(S1Msg) of
		ok -> ok; %%logger:log_msg("Sent Radio Capability Match Resposne ~w~n", [S1Msg]);
		{error, Reason} ->
				io:format("S1 Msg send failed ~w~n", [Reason]),
				error(Reason)
	end,
	{next_state, starting, StateData};


registered(Event, StateData) ->
	io:format("~nUknown Event in Registgered State ~w~n", [Event]),
	{next_state, registered, StateData}.


%% state_name/3
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_fsm.html#Module:StateName-3">gen_fsm:StateName/3</a>
-spec state_name(Event :: term(), From :: {pid(), Tag :: term()}, StateData :: term()) -> Result when
	Result :: {reply, Reply, NextStateName, NewStateData}
			| {reply, Reply, NextStateName, NewStateData, Timeout}
			| {reply, Reply, NextStateName, NewStateData, hibernate}
			| {next_state, NextStateName, NewStateData}
			| {next_state, NextStateName, NewStateData, Timeout}
			| {next_state, NextStateName, NewStateData, hibernate}
			| {stop, Reason, Reply, NewStateData}
			| {stop, Reason, NewStateData},
	Reply :: term(),
	NextStateName :: atom(),
	NewStateData :: atom(),
	Timeout :: non_neg_integer() | infinity,
	Reason :: normal | term().
%% ====================================================================
state_name(Event, From, StateData) ->
    Reply = ok,
    {reply, Reply, state_name, StateData}.


%% handle_event/3
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_fsm.html#Module:handle_event-3">gen_fsm:handle_event/3</a>
-spec handle_event(Event :: term(), StateName :: atom(), StateData :: term()) -> Result when
	Result :: {next_state, NextStateName, NewStateData}
			| {next_state, NextStateName, NewStateData, Timeout}
			| {next_state, NextStateName, NewStateData, hibernate}
			| {stop, Reason, NewStateData},
	NextStateName :: atom(),
	NewStateData :: term(),
	Timeout :: non_neg_integer() | infinity,
	Reason :: term().
%% ====================================================================
handle_event(Event, StateName, StateData) ->
    {next_state, StateName, StateData}.


%% handle_sync_event/4
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_fsm.html#Module:handle_sync_event-4">gen_fsm:handle_sync_event/4</a>
-spec handle_sync_event(Event :: term(), From :: {pid(), Tag :: term()}, StateName :: atom(), StateData :: term()) -> Result when
	Result :: {reply, Reply, NextStateName, NewStateData}
			| {reply, Reply, NextStateName, NewStateData, Timeout}
			| {reply, Reply, NextStateName, NewStateData, hibernate}
			| {next_state, NextStateName, NewStateData}
			| {next_state, NextStateName, NewStateData, Timeout}
			| {next_state, NextStateName, NewStateData, hibernate}
			| {stop, Reason, Reply, NewStateData}
			| {stop, Reason, NewStateData},
	Reply :: term(),
	NextStateName :: atom(),
	NewStateData :: term(),
	Timeout :: non_neg_integer() | infinity,
	Reason :: term().
%% ====================================================================
handle_sync_event(Event, From, StateName, StateData) ->
    Reply = ok,
    {reply, Reply, StateName, StateData}.


%% handle_info/3
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_fsm.html#Module:handle_info-3">gen_fsm:handle_info/3</a>
-spec handle_info(Info :: term(), StateName :: atom(), StateData :: term()) -> Result when
	Result :: {next_state, NextStateName, NewStateData}
			| {next_state, NextStateName, NewStateData, Timeout}
			| {next_state, NextStateName, NewStateData, hibernate}
			| {stop, Reason, NewStateData},
	NextStateName :: atom(),
	NewStateData :: term(),
	Timeout :: non_neg_integer() | infinity,
	Reason :: normal | term().
%% ====================================================================
handle_info(Info, StateName, StateData) ->
    {next_state, StateName, StateData}.


%% terminate/3
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_fsm.html#Module:terminate-3">gen_fsm:terminate/3</a>
-spec terminate(Reason, StateName :: atom(), StateData :: term()) -> Result :: term() when
	Reason :: normal
			| shutdown
			| {shutdown, term()}
			| term().
%% ====================================================================
terminate(Reason, StateName, StatData) ->
    ok.


%% code_change/4
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_fsm.html#Module:code_change-4">gen_fsm:code_change/4</a>
-spec code_change(OldVsn, StateName :: atom(), StateData :: term(), Extra :: term()) -> {ok, NextStateName :: atom(), NewStateData :: term()} when
	OldVsn :: Vsn | {down, Vsn},
	Vsn :: term().
%% ====================================================================
code_change(OldVsn, StateName, StateData, Extra) ->
    {ok, StateName, StateData}.


%% ====================================================================
%% Internal functions
%% ====================================================================
process_nas_pdu(<<_:4, ?PD_MM:4, ?AUTHENTICATION_REQUEST:8, 
				  	_:4, Ksi:4, Rand:128, AutnLen:8, Autn:128>>, StateData) ->
	
	Msg = s1codec:encS1Msg( [ {?'id-MME-UE-S1AP-ID', reject, mandatory, 
						 	  		ets:lookup_element(StateData#state.ueTblRef, StateData#state.ueS1apId, 3)},
							  {?'id-eNB-UE-S1AP-ID' , reject, mandatory, StateData#state.ueS1apId},
							  {?'id-NAS-PDU', reject, mandatory, 
										nas_codec:mk_auth_response(gen_auth_res(Ksi, Rand, AutnLen, Autn, StateData#state.secInfo#ueSecInfo.sharedKey))},
							  {?'id-EUTRAN-CGI', ignore, mandatory, s1_hlpr:s1ap_ie_val(cgi, StateData#state.ueConfig)}, 
							  {?'id-TAI', ignore, mandatory, s1_hlpr:s1ap_ie_val(tai, StateData#state.ueConfig)}
				  	 		], 
					 		initiatingMessage, 'uplinkNASTransport', ?'id-uplinkNASTransport', ignore),
	(StateData#state.enb_snd_fn)(Msg),
	{next_state, deregistered, StateData};
	


process_nas_pdu(<<_:4, ?PD_MM:4, ?IDENTITY_REQUEST:8, _:4, ?IMSI_IDENTITY:4>>, StateData) ->
	(StateData#state.enb_snd_fn)(s1codec:encS1Msg( [ { ?'id-MME-UE-S1AP-ID', reject, mandatory, 
						 	  							ets:lookup_element(StateData#state.ueTblRef, StateData#state.ueS1apId, 3)},
							  						{?'id-eNB-UE-S1AP-ID' , reject, mandatory, StateData#state.ueS1apId},
							  						{?'id-NAS-PDU', reject, mandatory, 
													 			list_to_bitstring( [
																					<<?PLAIN_NAS:4>>, <<?PD_MM:4>>,
															   						<<?IDENTITY_RESPONSE:8>>,
																					mk_ueitem_ie(StateData#state.ueConfig, imsi)  ]
																				 )},
													{?'id-EUTRAN-CGI', ignore, mandatory, s1_hlpr:s1ap_ie_val(cgi, StateData#state.ueConfig)}, 
							  						{?'id-TAI', ignore, mandatory, s1_hlpr:s1ap_ie_val(tai, StateData#state.ueConfig)}
												],
												 initiatingMessage, 'uplinkNASTransport', ?'id-uplinkNASTransport', ignore)
					),
	{next_state, deregistered, StateData};


process_nas_pdu(<<3:4, 7:4,  _MAC:32, SeqNum:8, _:4, ?PD_MM:4, ?SECURITY_MODE_COMMAND:8, Cipher:4, Integrity:4, _:5,
				  	 NasKsi:3, UeCapLen:8, UeCap:UeCapLen/unit:8, R/bitstring>>, StateData) ->
	
	(StateData#state.enb_snd_fn)(s1codec:encS1Msg( [ { ?'id-MME-UE-S1AP-ID', reject, mandatory, 
						 	  							ets:lookup_element(StateData#state.ueTblRef, StateData#state.ueS1apId, 3)},
							  						{?'id-eNB-UE-S1AP-ID' , reject, mandatory, StateData#state.ueS1apId},
							  						{?'id-NAS-PDU', reject, mandatory, 
													 		nas_codec:mk_secure_nas_pdu(StateData#state.secInfo#ueSecInfo{uplinkSeqNum = 0}, 
																					[<<?PLAIN_NAS:4>>, <<?PD_MM:4>>,
															   						<<?SECURITY_MODE_COMPLETE:8>>,
																					(send_imeisv(StateData, R))] ) },
													{?'id-EUTRAN-CGI', ignore, mandatory, s1_hlpr:s1ap_ie_val(cgi, StateData#state.ueConfig)}, 
							  						{?'id-TAI', ignore, mandatory, s1_hlpr:s1ap_ie_val(tai, StateData#state.ueConfig)}
												],
												 initiatingMessage, 'uplinkNASTransport', ?'id-uplinkNASTransport', ignore)
					),
	{next_state, deregistered, StateData#state{secInfo = StateData#state.secInfo#ueSecInfo{uplinkSeqNum = 0},
											   	nasKsi = NasKsi,
												nasCipher = Cipher,
												nasIntegrity = Integrity}};

process_nas_pdu(<<3:4, 7:4,  _MAC:32, SeqNum:8, _:4, ?PD_MM:4, ?IDENTITY_REQUEST:8, _:4, ?IMEISV_IDENTITY:4>>, StateData) ->

	(StateData#state.enb_snd_fn)(s1codec:encS1Msg( [ { ?'id-MME-UE-S1AP-ID', reject, mandatory, 
						 	  							ets:lookup_element(StateData#state.ueTblRef, StateData#state.ueS1apId, 3)},
							  						{?'id-eNB-UE-S1AP-ID' , reject, mandatory, StateData#state.ueS1apId},
							  						{?'id-NAS-PDU', reject, mandatory, 
													 			nas_codec:mk_secure_nas_pdu(StateData#state.secInfo, 
																					[<<?PLAIN_NAS:4>>, <<?PD_MM:4>>,
															   						<<?IDENTITY_RESPONSE:8>>,
																					(mk_ueitem_ie(StateData, imei))] ) },
													{?'id-EUTRAN-CGI', ignore, mandatory, s1_hlpr:s1ap_ie_val(cgi, StateData#state.ueConfig)}, 
							  						{?'id-TAI', ignore, mandatory, s1_hlpr:s1ap_ie_val(tai, StateData#state.ueConfig)}
												],
												 initiatingMessage, 'uplinkNASTransport', ?'id-uplinkNASTransport', ignore)
								),
	{next_state, deregistered, StateData#state{secInfo = StateData#state.secInfo#ueSecInfo{uplinkSeqNum = 
																						  StateData#state.secInfo#ueSecInfo.uplinkSeqNum + 1}}};


process_nas_pdu(<<3:4, 7:4,  _MAC:32, SeqNum:8, _:4, ?PD_MM:4, ?IDENTITY_REQUEST:8, _:4, ?IMSI_IDENTITY:4>>, StateData) -> 

	(StateData#state.enb_snd_fn)(s1codec:encS1Msg( [ { ?'id-MME-UE-S1AP-ID', reject, mandatory, 
						 	  							ets:lookup_element(StateData#state.ueTblRef, StateData#state.ueS1apId, 3)},
							  						{?'id-eNB-UE-S1AP-ID' , reject, mandatory, StateData#state.ueS1apId},
							  						{?'id-NAS-PDU', reject, mandatory, 
													 			mk_secure_nas_pdu(StateData#state.secInfo, 
																					[<<?PLAIN_NAS:4>>, <<?PD_MM:4>>,
															   						<<?IDENTITY_RESPONSE:8>>,
																					(mk_ueitem_ie(StateData, imsi))] ) },
													{?'id-EUTRAN-CGI', ignore, mandatory, s1_hlpr:s1ap_ie_val(cgi, StateData#state.ueConfig)}, 
							  						{?'id-TAI', ignore, mandatory, s1_hlpr:s1ap_ie_val(tai, StateData#state.ueConfig)}
												],
												 initiatingMessage, 'uplinkNASTransport', ?'id-uplinkNASTransport', ignore)
									),
	{next_state, deregistered, StateData#state{secInfo = StateData#state.secInfo#ueSecInfo{uplinkSeqNum = 
																						  StateData#state.secInfo#ueSecInfo.uplinkSeqNum + 1}}};


process_nas_pdu(<<3:4, 7:4,  _MAC:32, SeqNum:8, _:4, ?PD_ESM:4, Pti:8, ?ESM_INFO_REQ:8>>, StateData) -> 

	(StateData#state.enb_snd_fn)( s1codec:encS1Msg( [ { ?'id-MME-UE-S1AP-ID', reject, mandatory, 
						 	  							ets:lookup_element(StateData#state.ueTblRef, StateData#state.ueS1apId, 3)},
							  						{?'id-eNB-UE-S1AP-ID' , reject, mandatory, StateData#state.ueS1apId},
							  						{?'id-NAS-PDU', reject, mandatory, 
													 			mk_secure_nas_pdu(StateData#state.secInfo, 
																					[<<?PLAIN_NAS:4>>, <<?PD_MM:4>>,
																					 <<Pti:8>>,
															   						<<?ESM_INFO_RESP:8>>,
																					(mk_ueitem_ie(StateData, apn)),
																					(mk_ueitem_ie(StateData, pco))
																					] ) },
													{?'id-EUTRAN-CGI', ignore, mandatory, s1_hlpr:s1ap_ie_val(cgi, StateData#state.ueConfig)}, 
							  						{?'id-TAI', ignore, mandatory, s1_hlpr:s1ap_ie_val(tai, StateData#state.ueConfig)}
												],
												 initiatingMessage, 'uplinkNASTransport', ?'id-uplinkNASTransport', ignore
											)
								),
	{next_state, deregistered, StateData#state{secInfo = StateData#state.secInfo#ueSecInfo{uplinkSeqNum = 
																						  StateData#state.secInfo#ueSecInfo.uplinkSeqNum + 1}}};


process_nas_pdu(<<3:4, 7:4,  _MAC:32, SeqNum:8, _:4, ?PD_MM:4, ?TRACKING_AREA_UPDATE_ACCEPT:8, _R/binary>>, StateData) ->
		S1Msg = s1codec:encS1Msg( [ { ?'id-MME-UE-S1AP-ID', reject, mandatory, 
								  						ets:lookup_element(StateData#state.ueTblRef, StateData#state.ueS1apId, 3)},
							  	{?'id-eNB-UE-S1AP-ID' , reject, mandatory, StateData#state.ueS1apId},
							  	{?'id-NAS-PDU', reject, mandatory, 
													 mk_secure_nas_pdu(StateData#state.secInfo, 
																	[	<<?PLAIN_NAS:4>>, <<?PD_MM:4>>,
															   			<<?TRACKING_AREA_UPDATE_COMPLETE:8>>		
																		] ) },
								{?'id-EUTRAN-CGI', ignore, mandatory, s1_hlpr:s1ap_ie_val(cgi, StateData#state.ueConfig)}, 
							  	{?'id-TAI', ignore, mandatory, s1_hlpr:s1ap_ie_val(tai, StateData#state.ueConfig)}
							  ],
							  initiatingMessage, 'uplinkNASTransport', ?'id-uplinkNASTransport', ignore),
	case (StateData#state.enb_snd_fn)(S1Msg) of
		ok -> ok; %%logger:log_msg"Tracking Area Update Complete ~w~n", [S1Msg]);
		{error, Reason} ->
				io:format("S1 Msg send failed ~w~n", [Reason]),
				error(Reason)
	end,
	{next_state, registered, StateData};

process_nas_pdu(<<3:4, 7:4,  _MAC:32, SeqNum:8, _:4, ?PD_MM:4, ?GUTI_REALLOCATION_COMMAND:8, 
				  	GutiLen:8, Guti:GutiLen/binary, R/binary>> = Msg, StateData) ->
	%%logger:log_msg("Got Guti Realloc ~w~n", [Msg]),
	S1Msg = s1codec:encS1Msg( [ { ?'id-MME-UE-S1AP-ID', reject, mandatory, 
								  						ets:lookup_element(StateData#state.ueTblRef, StateData#state.ueS1apId, 3)},
							  	{?'id-eNB-UE-S1AP-ID' , reject, mandatory, StateData#state.ueS1apId},
							  	{?'id-NAS-PDU', reject, mandatory, 
													 mk_secure_nas_pdu(StateData#state.secInfo, 
																	[	<<?PLAIN_NAS:4>>, <<?PD_MM:4>>,
															   			<<?GUTI_REALLOCATION_COMPLETE:8>>		
																		] ) },
								{?'id-EUTRAN-CGI', ignore, mandatory, s1_hlpr:s1ap_ie_val(cgi, StateData#state.ueConfig)}, 
							  	{?'id-TAI', ignore, mandatory, s1_hlpr:s1ap_ie_val(tai, StateData#state.ueConfig)}
							  ],
							  initiatingMessage, 'uplinkNASTransport', ?'id-uplinkNASTransport', ignore),
	case (StateData#state.enb_snd_fn)(S1Msg) of
		ok -> ok; %% logger:log_msg(" Sent GUIT Realloc complete  ~w~n", [S1Msg]);
		{error, Reason} ->
				io:format("S1 Msg send failed ~w~n", [Reason]),
				error(Reason)
	end,
	{next_state, registered, 
	 	StateData#state{attachParams = 
						    update_attachParams(StateData#state.attachParams, [{?GUTI_IEI, Guti} | nas_codec:get_optional_ies(R)] )}};


process_nas_pdu(<<3:4, 7:4,  _MAC:32, SeqNum:8, _:4, ?PD_MM:4, ?DETACH_REQUEST:8, _:4, DetachType:4,  R/binary>> = Msg, StateData) ->
	%%logger:log_msg("Got Detach Req ~w~n", [Msg]),
	S1Msg = s1codec:encS1Msg( [ { ?'id-MME-UE-S1AP-ID', reject, mandatory, 
								  						ets:lookup_element(StateData#state.ueTblRef, StateData#state.ueS1apId, 3)},
							  	{?'id-eNB-UE-S1AP-ID' , reject, mandatory, StateData#state.ueS1apId},
							  	{?'id-NAS-PDU', reject, mandatory, 
													 mk_secure_nas_pdu(StateData#state.secInfo, 
																	[	<<?PLAIN_NAS:4>>, <<?PD_MM:4>>,
															   			<<?DETACH_ACCEPT:8>>		
																		] ) },
								{?'id-EUTRAN-CGI', ignore, mandatory, s1_hlpr:s1ap_ie_val(cgi, StateData#state.ueConfig)}, 
							  	{?'id-TAI', ignore, mandatory, s1_hlpr:s1ap_ie_val(tai, StateData#state.ueConfig)}
							  ],
							  initiatingMessage, 'uplinkNASTransport', ?'id-uplinkNASTransport', ignore),
	case (StateData#state.enb_snd_fn)(S1Msg) of
		ok -> ok; %%logger:log_msg(" Sent Detach Accept  ~w~n", [S1Msg]);
		{error, Reason} ->
				io:format("S1 Msg send failed ~w~n", [Reason]),
				error(Reason)
	end,
	gen_fsm:cancel_timer(StateData#state.t3412Ref),
	{next_state, deregistered, StateData};




process_nas_pdu(Msg, StateData) ->
	io:format("Unknown Nas msg rcvd ~w~n", [Msg]),
	{next_state, deregistered, StateData}.



validate_initial_context_setup( StateData, IeList) ->
	%%io:format("validate init context msg ~w~n", [IeList]), timer:sleep(10),
	Params = #cxtParams{},
	try
		ERABPar = case lists:keyfind(?'id-E-RABToBeSetupListCtxtSUReq', 2, IeList) of
			{_, _, _, [{_, _, _, {_, ErabId, {_, Qci, {_, _, ArpPrio, ArpVul, _}, _, _}, GwIpAddr, GtpTid, NasPdu, _}}]} = Ie -> 
				%%io:format("E-RABCtxtSU Req ~w~n", [Ie]),
				[{?ERAB_ID, ErabId}, {?ARP_PRIO, ArpPrio}, {?ARP_VUL, ArpVul}, {?GW_IP_ADDR, GwIpAddr}, {?GTP_TID, GtpTid}] ++ 
															nas_codec:parse_nas_pdu(NasPdu);
			false -> throw(noIe)
		end,
		BitRate = case  lists:keyfind(?'id-uEaggregateMaximumBitrate', 2, IeList)  of
			{_, _, _, {_, DLrate, ULrate, _}} -> [{dlRate, DLrate}, {ulRate, ULrate}];
			false -> throw(noIe)  %% [{dlRate, 0}, {ulRate, 0}]
		end,
		{ok, ERABPar ++ BitRate}
	catch
		throw:noIe ->
			{notok, Params}
	end.

		

send_initial_context_response(StateData) ->
	Msg = s1codec:encS1Msg( 
					   			[	{?'id-MME-UE-S1AP-ID', ignore, mandatory,
									 	 ets:lookup_element(StateData#state.ueTblRef, StateData#state.ueS1apId, 3)},
									{?'id-eNB-UE-S1AP-ID' , ignore, mandatory, StateData#state.ueS1apId},
									{?'id-E-RABSetupListCtxtSURes', ignore, mandatory, 
											[#'E-RABSetupListCtxtSURes_SEQOF'{
												 id = ?'id-E-RABSetupItemCtxtSURes', 
												criticality = ignore,
												value = #'E-RABSetupItemCtxtSURes'{
													'e-RAB-ID'  = get_param(StateData#state.attachParams, ?ERAB_ID),
													transportLayerAddress = get_param(StateData#state.attachParams, ?GW_IP_ADDR), 
													'gTP-TEID' = get_param(StateData#state.attachParams, ?GTP_TID)
												}
											}]
									}
				  				], 
						successfulOutcome, 'initialContextSetupResponse', ?'id-InitialContextSetup', reject),
	case (StateData#state.enb_snd_fn)(Msg) of
		ok -> ok;
		{error, Reason} ->
			io:format("S1 Msg send failed ~w~n", [Reason]),
			error(Reason)
	end.



send_ueCapabilityIndication(StateData) ->
	 Msg = s1codec:encS1Msg(    [	{?'id-MME-UE-S1AP-ID', reject, mandatory,
									 	 ets:lookup_element(StateData#state.ueTblRef, StateData#state.ueS1apId, 3)},
									{?'id-eNB-UE-S1AP-ID' , reject, mandatory, StateData#state.ueS1apId},
									{?'id-UERadioCapability', ignore, mandatory, ueRadioCapabilities(StateData)}
								],
								initiatingMessage, 'uECapabilityInfoIndication', ?'id-UECapabilityInfoIndication', ignore),
	 case (StateData#state.enb_snd_fn)(Msg) of
		ok -> ok;
		{error, Reason} ->
			io:format("S1 Msg send failed ~w~n", [Reason]),
			error(Reason)
	end.
		




send_initial_context_failure(StateDate, CtxtParams) ->
	ok.


gen_auth_res(Ksi, Rand, AutnLen, Autn, Mykey) ->
	<<12345678:32>>.

mk_nas_pdu(StateData, IeList) ->
	%%io:format("Nas Pdu~n"),
	%%s1_hlpr:hex_print(list_to_bitstring( [<<?PLAIN_NAS:4>>, <<?PD_MM:4>>, IeList])),
	list_to_bitstring( [<<?PLAIN_NAS:4>>, <<?PD_MM:4>>, IeList]).

mk_secure_nas_pdu(StateData, IeList) ->
	%%io:format("IeList ~w~n State: ~w~n", [IeList, StateData]),timer:sleep(10),
	list_to_bitstring( [<<?INTEGRITY_PROTECTED_EPS_NAS:4>>, <<?PD_MM:4>>,
						 <<(mkMAC(StateData, IeList)):32>>, <<(StateData#ueSecInfo.uplinkSeqNum):8>>,
						IeList]).


mkMAC(StateData, IeList) ->
  123456.


mk_esm(BearerId,  IeList) ->
	list_to_bitstring([BearerId, <<?PD_ESM:4>>, IeList]).

send_imeisv(StateData, <<>>)->
	[];
send_imeisv(StateData, <<16#C:4, 0:1, 2#001:3, _/bitstring>>) ->
	%%io:format("imei val ~w~n", [[<<16#23:4>>, s1_hlpr:ie(imei, proplists:get_value(imei, StateData#state.ueConfig, []))]]),
	[<<16#23:4>>, s1_hlpr:ie(imei, proplists:get_value(imei, StateData#state.ueConfig, []))].


mk_ueitem_ie(StateData, Item) ->
	%%io:format("~w val ~w~n:", [Item, s1_hlpr:ie(Item, proplists:get_value(Item, StateData#state.ueConfig, []))]),
	s1_hlpr:ie(Item, proplists:get_value(Item, StateData#state.ueConfig, [])).





get_param(ParamList, Param) ->
	case lists:keyfind(Param, 1, ParamList) of
		{_, Val} -> Val;
		_ -> 
			%%io:format("attach Param not found ~w~n", [Param]),
			[]
	end.

t3412Timer(StateData) ->
	case get_param(StateData#state.attachParams, ?T3412) of
		[] -> StateData#state.defaultT3412Timer;
		Val -> Val
	end.

guti(StateData) ->
	G = case get_param(StateData#state.attachParams, ?GUTI_IEI) of
		[] -> <<1:8>>;
		Val -> Val
	end,
	list_to_bitstring([<<?GUTI_IEI:8>>,  <<(byte_size(G)):8>>, G]).


ueRadioCapabilities(_StateData) -> <<1234567890>>.


update_attachParams(Attachparams, []) -> Attachparams;
update_attachParams(Attachparams, [{K, _V} = T | NewList]) ->
	update_attachParams(lists:keystore(K, 1, Attachparams, T), NewList).

