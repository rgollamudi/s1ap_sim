%% @author rgollamudi
%% @doc @todo Add description to nas_codec.


-module(nas_codec).


-include("nas.hrl").

%% ====================================================================
%% API functions
%% ====================================================================
-export([mk_auth_response/1, get_imsi/1, get_netCapabilities/1, mk_secure_nas_pdu/2, parse_nas_pdu/1, get_optional_ies/1]).


mk_auth_response(AuthRespParam) ->
	list_to_bitstring([<<?PLAIN_NAS:4>>, <<?PD_MM:4>>, <<?AUTHENTICATION_RESPONSE:8>>, <<(erlang:byte_size(AuthRespParam)):8>>, 
													AuthRespParam]).

get_imsi(<<_:24, L:8, B1:4, Ind:1, _:3, I/bitstring>>) ->
	Len = (L-1)*8,
	<<ImsiBytes:Len/bitstring, _/bitstring>> = I,
	case Ind of
		1 -> list_to_bitstring( [<<B1:4>> | [ <<X1:4, X2:4>> || <<X2:4, X1:4>> <= ImsiBytes] ]);
		0 ->
			A = Len - 8,
			<<ImsiB2:A/bits, _:4, Lb:4>> = ImsiBytes,
			list_to_bitstring([<<B1:4>> | [ <<X1:4, X2:4>> || <<X2:4, X1:4>> <= ImsiB2] ++ [<<Lb:4>>]])
	end.
	
get_netCapabilities(<<_:24, MobileIdLen:8,  I/bitstring>>) ->
	<<_:MobileIdLen/unit:8, UeCapLen:8, UeCapability:UeCapLen/unit:8, _/bitstring>> = I,
	%%logger:log_msg("UE Cap Len: ~w ,Ue Capability: ~.16B~n ", [ UeCapLen, UeCapability]),
	<<UeCapability:UeCapLen/unit:8>>.

mk_secure_nas_pdu(SecInfo, IeList) ->
	%%io:format("StateData ~w~n", [StateData]),timer:sleep(10),
	list_to_bitstring( [<<?INTEGRITY_PROTECTED_EPS_NAS:4>>, <<?PD_MM:4>>,
						 <<(mkMAC(SecInfo, IeList)):32>>, <<(SecInfo#ueSecInfo.uplinkSeqNum):8>>,
						IeList]).


parse_nas_pdu(<<SecHdr:4, PD:4, _/bitstring>> = NasPdu) ->
	case <<SecHdr, PD>> of
		<<?PLAIN_NAS:4, ?PD_MM:4>> ->
			<<_:8, Msg/bitstring>> = NasPdu,
			parse_emm_pdu(Msg);
		<<?PLAIN_NAS:4, ?PD_ESM:4>> ->
			<<_:8, _Pti:8, Msg/bitstring>> = NasPdu,
			parse_esm_pdu(Msg);
		<<?INTEGRITY_PROTECTED_NAS:4, ?PD_MM:4>> ->
			<<_:40, _SeqNum:8, _:8, Pdu/bitstring>> = NasPdu,
			parse_emm_pdu(Pdu);
		%%<<?INTEGRITY_PROTECTED_EPS_NAS:4, ?PD_MM:4>> ->
		<<?INTEGRITY_PROTECTED_EPS_NAS, ?PD_MM>> ->
			<<_:40, _SeqNum:8, _:8, Pdu/bitstring>> = NasPdu,
			parse_emm_pdu(Pdu);
		<<?INTEGRITY_PROTECTED_EPS_NAS:4, ?PD_ESM:4>> ->
			<<_:40, _SeqNum:8, _:8, _Pti:8, Pdu/bitstring>> = NasPdu,
			parse_emm_pdu(Pdu);
		_ ->
			io:format("~nunknonw NAS Type ~w~w~n", [SecHdr, PD])
	end.



		
		
		

%% ====================================================================
%% Internal functions
%% ====================================================================


parse_emm_pdu(<<?ATTACH_REQUEST:8, Msg/bitstring>>) ->
	ok;

parse_emm_pdu(<<?ATTACH_ACCEPT:8, R/binary>>) ->


	<<_:4, Res:4, TimeType:3, GprsTimer:5, TaiLen:8, Tai:TaiLen/binary, EsmLen:16, Esm:EsmLen/binary, R1/binary>> = R,
	<<EpsBearerId:4, _:4, Pti:8, _:8, QosLen:8, Qos:QosLen/binary, ApnLen:8, Apn:ApnLen/binary,
	  							PdnAddrLen:8, PdnAddrC:PdnAddrLen/binary,  R2/binary>> = Esm,
	T3412 = case TimeType of						%% translating everthing to msecs
				2#000 -> GprsTimer * 2 * 1000;
				2#001 -> GprsTimer * 60 * 1000;				
				2#010 -> GprsTimer * 6 * 60 * 1000;			%% decihour = 6 minutes
				2#111 -> infinity;
				_ ->  GprsTimer * 60 * 1000
			end,
	EsmIes = get_optional_ies(R2),
%% io:format("EmsIes ~w~n", [EsmIes]),
	Pco = case lists:keyfind(?PCO, 1, EsmIes) of
			  false -> [];
			  PcoIe ->
%%				  io:format("PCO ~w~n", [PcoIe]),
				  {_, Iepco} = PcoIe,
%%				  s1_hlpr:hex_print(Iepco),
				  parse_pco(Iepco)
		  end,
%%io:format("Pco ~w~n", [Pco]),
 
	<<_:8, PdnAddr/binary>> = PdnAddrC,
	
	[{?ATTACH_RESULT, Res}, {?T3412, T3412}, {?TAI_LIST, Tai}, 
	 	{?EPS_BEARER_ID, EpsBearerId}, {?PTI, Pti}, {?EPS_QOS, Qos}, {?APN_NAME, Apn}, {?PDN_ADDR, PdnAddr}] 
													++ get_optional_ies(R1) ++ EsmIes ++ Pco;


parse_emm_pdu(<<Req:8, Msg/bitstring>>) ->
	io:format("~nDid not recognize NAS PDU type~w ~w~n", [Req, Msg]).

parse_esm_pdu(Msg) -> [].			%% for now.


get_optional_ies(R1) ->
	get_optional_ies([], R1).


get_optional_ies(Ies, <<>>) ->
	Ies;
get_optional_ies(Ies, <<_:8, _X/binary>>=Msg) ->
	%%io:format("ie Mas ~w~n", [Msg]),
	{IeType, Ie, R} = extract_ie(Msg),
	%%io:format("IeType ~w, Ie ~w, R=~w~n~n", [IeType, Ie, R]),
	get_optional_ies(Ies ++ [{IeType, Ie}], R).
	
%%extract_ie(<<?GUTI_IEI:8, L:8, Ie:L/binary, R/binary>> = Msg) when byte_size(Msg) >= 13 ->
extract_ie(<<?GUTI_IEI:8, L:8, Ie:L/binary, R/binary>> = Msg) ->
	{?GUTI_IEI, Ie, R};
extract_ie(<<?LAI_IEI:8, Ie:8, R/binary>>= Msg) when byte_size(Msg) >= 6   ->  {?LAI_IEI, Ie, R};
extract_ie(<<?MS_ID_IEI:8, L:8, Ie:L/binary, R/binary>> = Msg) when byte_size(Msg) >= 7 ->
	{?MS_ID_IEI, Ie, R};
extract_ie(<<?LAI_IEI:8, Ie:8, R/binary>>= Msg) when byte_size(Msg) >= 2  ->  {?LAI_IEI, Ie, R};
extract_ie(<<?T3402_IEI:8, Ie:8, R/binary>>= Msg) when byte_size(Msg) >= 2  ->  {?T3402_IEI, Ie, R};
extract_ie(<<?T3423_IEI:8, Ie:8, R/binary>>= Msg) when byte_size(Msg) >= 2  ->  {?T3423_IEI, Ie, R};
extract_ie(<<?EPLMS_IEI:8, L:8, Ie:L/binary, R/binary>>= Msg) when byte_size(Msg) >= 5  ->  {?EPLMS_IEI, Ie, R};
extract_ie(<<?EPS_NTWK_IEI:8, L:8, Ie:L/binary, R/binary>>= Msg) when byte_size(Msg) >= 3  ->  {?EPS_NTWK_IEI, Ie, R};
extract_ie(<<?ADD_RES_IEI:4, Ie:4, R/binary>>= Msg) when byte_size(Msg) >= 2  ->  {?ADD_RES_IEI, Ie, R};
extract_ie(<<?T3412_EXT_IEI:8, L:8, Ie:L/binary, R/binary>>= Msg) when byte_size(Msg) >= 3  ->  {?T3412_EXT_IEI, Ie, R};
extract_ie(<<?T3324_IEI:8, Ie:8, R/binary>>= Msg) when byte_size(Msg) >= 2  ->  {?T3324_IEI, Ie, R};
extract_ie(<<?ESM_TRANS_ID:8, L:8, Ie:L/binary, R/binary>>= Msg) when byte_size(Msg) >= 5  ->  {?ESM_TRANS_ID, Ie, R};
extract_ie(<<?NEGOTIATED_QOS:8, L:8, Ie:L/binary, R/binary>>= Msg) when byte_size(Msg) >= 16  ->  {?NEGOTIATED_QOS, Ie, R};
extract_ie(<<?NEGOTIATED_LLC_SAPI:8, Ie:8, R/binary>>= Msg) when byte_size(Msg) >= 2  ->  {?NEGOTIATED_LLC_SAPI, Ie, R};
extract_ie(<<?RADIO_PRIO:4, Ie:4, R/binary>>= Msg) when byte_size(Msg) >= 1  ->  {?RADIO_PRIO, Ie, R};
extract_ie(<<?PKT_FLOW_ID:8, L:8, Ie:L/binary, R/binary>>= Msg) when byte_size(Msg) >= 5  ->  {?PKT_FLOW_ID, Ie, R};
extract_ie(<<?PCO:8, L:8, Ie:L/binary, R/binary>>= Msg) when byte_size(Msg) >= 5  ->  {?PCO, Ie, R};
extract_ie(<<?APN_AMBR:8, L:8, Ie:L/binary, R/binary>>= Msg) when byte_size(Msg) >= 5  ->  {?APN_AMBR, Ie, R};
extract_ie(<<?ESM_CAUSE:4, Ie:4, R/binary>>= Msg) when byte_size(Msg) >= 1  ->  {?ESM_CAUSE, Ie, R};

extract_ie(_Msg) -> 
	%%io:format("Ie not found ~w~n", [_Msg]),
	%%s1_hlpr:hex_print(_Msg),
	{notok, notok, <<>>}.


parse_pco(<<1:1, _:4, 0:3, Ie/binary>> = PcoIe) when byte_size(PcoIe) >= 3 ->
	[{?PCO, PcoIe}];					%% for now.
parse_pco(_Msg) -> [].

mkMAC(SecInfo, IeList) ->
  123456.
