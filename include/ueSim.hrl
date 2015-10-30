%%%
%%% copyright Ramana Gollamudi
%%%




%%
%% UeTblRef tuple is as follows
%% {enbUeS1apId, UeSimPid, mmeUeS1apId, ueInfo},  The key is enbUeS1apId
%%


%%
%% enbTblRef tuple is as follows
%% {enbName, enbS1Pid, s1Sock, #enbInfo}.  The key is enbName
%%



-record(enbInfo, {name,
				  s1Addr,		%% s1 address are a record of S1 peer IP address, port, and source Ip address, port 
				  tai,
				  cgi,
				  rrcEstCause,
				  gwTlAddr,
				  relayNodeInd,
				  siptoGwAddr,
				  lhnId}).


-record(s1Addr, {s1PeerAddr,
				 s1PeerPort,
				 s1SelfAddr,
				 s1SelfPort}).


-define(S1NOTSETUP,				s1NotSetup).
-define(S1SETUP,				s1Setup).

-record(ueInfo, {state,
				 enbUeS1apId,
				 mmeUeS1apId,
				 connected_to,
				 initialUeMsgParams,
				 authParams,
				 timeout = 10}).


-record(ueParams, {connect_to,
				   ueS1apId,
				   ueTblRef,
				   enbTblRef,
				   ueConfig}).