-module('s1codec').

-include("s1ap.hrl").



-export([
		encodeS1Msg/5,
		encS1Msg/5
		]).



encS1Msg(MsgIes, MsgType, MsgName, MsgProcedureCode, MsgCriticatlity) ->
	case encodeS1Msg( MsgType, MsgName, MsgProcedureCode, MsgCriticatlity, MsgIes) of
		{ok, S1Msg} -> S1Msg;
		Error -> throw({"ASN1 encoding error", Error})
	end.




encodeS1Msg( MsgType, MsgName, MsgProcedureCode, MsgCriticatlity, MsgIes) ->
%%io:format("~nin encodeS1msg~w~n", [MsgIes]), timer:sleep(10),
	IeList = lists:foldl(fun({IeId, IeCriticality, _, IeVal}, Acc) -> Acc ++ 
								case IeVal of
									[] -> [];
									_ -> [ #'ProtocolIE-Field' {id = IeId, 
																criticality = IeCriticality,
																value = IeVal}]
								end 
						 end, [], MsgIes),
	MsgVal = {MsgName, IeList},
	%%io:format("~nIe list2 ~w~n", [MsgVal]),
	%%timer:sleep(10),
	Msg = {get_msg_type_name(MsgType), MsgProcedureCode, MsgCriticatlity, MsgVal},
	s1ap:encode('S1AP-PDU', {MsgType, Msg}).



get_msg_type_name(initiatingMessage) -> 'InitiatingMessage';
get_msg_type_name(successfulOutcome) -> 'SuccessfulOutcome';
get_msg_type_name(unsuccessfulOutcome) -> 'UnsuccessfulOutcome'.



