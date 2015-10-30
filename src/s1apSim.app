{application, s1apSim,
	[{vsn, "0.1.0"},
	 {modules, [enb_s1_sup, enb_s1, logger, nas_codec, s1_hlpr, s1ap, s1apSim_starter,
				s1apSim_sup, s1codec, test_interpreter, ue_sim_sup, ue_sim]},
	 {registered, [ue_sup, enb_sup]},
	 {mod, {s1apSim, []}}
	]}.