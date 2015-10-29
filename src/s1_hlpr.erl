%% @author rgollamudi
%% @doc @todo Add description to s1_hlpr.


-module(s1_hlpr).


-include("s1ap.hrl").



-define(EPS_MOBILE_ID_IEI,		16#17).
-define(IMSI_TYPE_IEI,				2#001).
-define(IMEI_TYPE_IEI,				2#011).
-define(UE_NETWORK_CAPABILITY_IEI,  2#00000000).		%% TBD where is this specified?
-define(ESM_MESSAGE_CONTAINER_IEI,  2#00000000).
-define(ESM_INFO_TRF_FLAG_IEI, 		16#D).
-define(APN_IEI,					28).
-define(PCO_IEI,					27).
-define(DEVICE_PROPERTIES_IEI,			16#C).



%% ====================================================================
%% API functions
%% ====================================================================
-export([send_s1_setup/2, send_initialUEMessage/2, send_authresp/2, s1ap_ie_val/2, ie/2, hex_print/1]).
-compile(export_all).


send_s1_setup(R,  EnbPid)  ->
	{ok, S1Msg} = s1codec:encodeS1Msg(initiatingMessage, 'S1SetupRequest', ?'id-S1Setup', reject,
							 			[{?'id-Global-ENB-ID', reject, mandatory, s1ap_ie_val(glbEnbId, R)}, 
											{?'id-eNBname', ignore, optional, s1ap_ie_val(enbName, R)}, 
											{?'id-SupportedTAs', reject, mandatory, s1ap_ie_val(supportedTA, R)},
											{?'id-DefaultPagingDRX', ignore, mandatory, s1ap_ie_val(pagingDrx, R)},
											{?'id-CSG-IdList', reject, optional, s1ap_ie_val(csgIdList, R)}]   ),
									 
	gen_server:cast(EnbPid, {s1msg, S1Msg}).
	


send_initialUEMessage(R, EnbPid) ->
	NasPdu = s1ap_ie_val(nasPdu, R),
	%%io:format("Nas Pdu = ~w~n", [NasPdu]),
	%%hex_print(NasPdu),
	{ok, S1Msg} = s1codec:encodeS1Msg(initiatingMessage, 'initialUEMessage', ?'id-initialUEMessage', ignore,
									  [{?'id-eNB-UE-S1AP-ID', reject, mandatory, s1ap_ie_val(enbUeS1apId, R)},
										{?'id-NAS-PDU', reject, mandatory, s1ap_ie_val(nasPdu, R)},
		    							{?'id-TAI', reject, mandatory,  s1ap_ie_val(tai, R)},
		   								{?'id-EUTRAN-CGI', ignore, mandatory, s1ap_ie_val(cgi, R)},
		   								{?'id-RRC-Establishment-Cause', ignore, mandatory, s1ap_ie_val(rrcEstCause, R)},
		   								{?'id-S-TMSI', reject, optional, s1ap_ie_val(stmsi, R)},
		   								{?'id-CSG-Id', reject, optional, s1ap_ie_val(csgId, R)},
		   								{?'id-GUMMEI-ID', reject,  optional, s1ap_ie_val(gummeiId, R)},
		   								{?'id-CellAccessMode', reject, optional, s1ap_ie_val(cellAccessMode, R)},
		   								{?'id-GW-TransportLayerAddress', ignore, optional, s1ap_ie_val(gwTlAddr, R)},
		   								{?'id-RelayNode-Indicator', reject, optional, s1ap_ie_val(relayNodeInd, R)},
		   								{?'id-GUMMEIType', ignore, optional, s1ap_ie_val(gummeiType, R)},
		   								{?'id-Tunnel-Information-for-BBF', ignore, optional, s1ap_ie_val(tunInfoBbf, R)},
		   								{?'id-SIPTO-L-GW-TransportLayerAddress', ignore, optional, s1ap_ie_val(siptoTlAddr, R)},
		   								{?'id-LHN-ID', ignore, optional, s1ap_ie_val(lhnId, R)}
		   							   ]
									 ),
	%%io:format("~nSending initial UE msg ~w~n", [S1Msg]),
	gen_server:cast(EnbPid, {s1msg, S1Msg}).


send_authresp(R, EnbPid) ->
	%%io:format("send auth resp R: ~w~n", [R]),
		{ok, Msg} = s1codec:encodeS1Msg( initiatingMessage, 'uplinkNASTransport', ?'id-uplinkNASTransport', ignore,
									   [ {?'id-MME-UE-S1AP-ID', reject, mandatory, s1ap_ie_val(mmeUeS1apId, R)},
							  		    {?'id-eNB-UE-S1AP-ID' , reject, mandatory, s1ap_ie_val(enbUeS1apId, R)},
							  			{?'id-NAS-PDU', reject, mandatory, s1ap_ie_val(nasPdu, R)},
							  			{?'id-EUTRAN-CGI', ignore, madatory, s1ap_ie_val(cgi, R)}, 
							  			{?'id-TAI', ignore, mandatory, s1ap_ie_val(tai, R)}
				  	 		] 
					 		),
		io:format("~nSending Auth Resp msg ~w~n", [Msg]),
		gen_server:cast(EnbPid, {s1msg, Msg}).



get_ie(Ie, IeList) ->
		case lists:keyfind(Ie, 2, IeList) of
		{_, _, _, Val} -> Val;
		_ -> 
			%%io:format("attach Param not found ~w~n", [Param]),
			[]
	end.

%% ====================================================================
%% Internal functions
%% ====================================================================



s1ap_ie_val(IeType, List) ->
	%%io:format("IeType to encode = ~w~n", [IeType]), timer:sleep(10),
	case IeType of
		glbEnbId ->
			#'Global-ENB-ID'{pLMNidentity =  find_val(plmnId, List), 'eNB-ID' = find_val(enbId, List)};

		supportedTA ->
			[#'SupportedTAs-Item'{tAC = find_val(tac, List), broadcastPLMNs = find_val(bdPlmnId, List)}];

		nasPdu ->
			bld_nas_pdu(find_val(nasPdu, List));

		tai -> 
			#'TAI'{pLMNidentity = find_val(plmnId, find_val(tai, List)), 
				   	tAC = find_val(tac, find_val(tai, List))};
		
		cgi ->
			#'EUTRAN-CGI'{pLMNidentity = find_val(plmnId, find_val(cgi, List)), 
				   			'cell-ID' = find_val(cellId, find_val(cgi, List))};
		_ ->
			find_val(IeType, List)
	end.



bld_nas_pdu(Params) ->
	list_to_bitstring(lists:foldr(fun nasie/2, [], Params)).
	%%io:format("msg ~w~n", [Msg]),

bld_esm_msg(E) ->
	list_to_bitstring(lists:foldr(fun nasie/2, [], E)).

nasie({IeType, V}, Acc) ->
	[ie(IeType, V) | Acc].

ie(IeType, Value) ->
	case IeType of
		esmMsg ->
			bld_esm_msg(Value);
		mobileId ->
			{MobileIdType, Id} = Value,
			case MobileIdType of
				imsi ->
					%%NumDigits = lists:foldl(fun (_, Acc) -> Acc + 1 end, 0, Id),
					case (length(Id) rem 2) of
						0 -> 
							A = [?IMSI_TYPE_IEI | Id];
						_ -> 
							A = [(2#1000 bor ?IMSI_TYPE_IEI) | Id]
					end,
					B = list_to_bitstring(nibble_swap(A, [])),
					%%io:format("len of B ~w~n", [byte_size(B)]),
					%%hex_print(B),
					[<<(byte_size(B)):8>>, B];
					
				guti -> ok;
				imei -> 
					%%NumDigits = lists:foldl(fun (_, Acc) -> Acc + 1 end, 0, Imsi),
					case (length(Id) rem 2) of
						0 -> 
							A = [?IMEI_TYPE_IEI | Id];
						_ -> 
							A = [(2#1000 bor ?IMEI_TYPE_IEI) | Id]
					end,
					B = list_to_bitstring(nibble_swap(A, [])),
					%%io:format("len of B ~w~n", [byte_size(B)]),
					%%hex_print(B),
					[<<(byte_size(B)):8>>, B]
			end;
		ueNetCapability ->
			[<<(byte_size(Value)):8>>, Value];
		esmInfoTrfFlag ->
			[<<?ESM_INFO_TRF_FLAG_IEI:4>>, <<Value:4>>];
		apn ->
			%%io:format("apn ie: ~w~n", [[<<?APN_IEI:8>>, <<(lists:sum([1 || X <- Value])):8>>, Value]]),
			[<<?APN_IEI:8>>, <<(length(Value)):8>>, Value];
		pco ->														%%  hardcoded for now
			[<<?PCO_IEI:8>>, <<7:8>>, <<1:1>>, <<0:7>>, <<16#000a:16>>, <<0:8>>, <<16#000d:16>>, <<0:8>>];
		devProp ->
			[<<?DEVICE_PROPERTIES_IEI:4>>, <<Value:4>>];
		imsi ->
			%%NumDigits = lists:foldl(fun (_, Acc) -> Acc + 1 end, 0, Value),
			case ((length(Value)) rem 2) of
						0 -> 
							A = [?IMSI_TYPE_IEI | Value];
						_ -> 
							A = [(2#1000 bor ?IMSI_TYPE_IEI) | Value]
			end,
			B = list_to_bitstring(nibble_swap(A, [])),
					%%io:format("len of B ~w~n", [byte_size(B)]),
					%%hex_print(B),
			[<<(byte_size(B)):8>>, B];
		imei ->
			%%NumDigits = lists:foldl(fun (_, Acc) -> Acc + 1 end, 0, Value),
			case ((length(Value)) rem 2) of
						0 -> 
							A = [?IMEI_TYPE_IEI | Value];
						_ -> 
							A = [(2#1000 bor ?IMEI_TYPE_IEI) | Value]
					end,
					B = list_to_bitstring(nibble_swap(A, [])),
					%%io:format("len of B ~w~n", [byte_size(B)]),
					%%hex_print(B),
					[<<(byte_size(B)):8>>, B];
		_ -> Value
	end.


	
find_val(Ie, List) ->
	case lists:keyfind(Ie, 1, List) of
		{_, P} -> P;
		false ->
			%%io:format("Ie not defined in conf file~w~n", [Ie]),
			[]
	end.



nibble_swap([], N) -> N;
nibble_swap([E|[]], N) ->  nibble_swap([], N ++ [<<2#1111:4>>] ++ [<<E:4>>]);
nibble_swap([E1 | [E2 | R]], N) ->  nibble_swap(R, N ++ [<<E2:4>>] ++ [<<E1:4>>]).

hex_print(Bits) ->
	L = bitstring_to_list(Bits),
	lists:map(fun(X) -> io:format("~.16b, ", [X]) end, L),
	io:format("~n").

