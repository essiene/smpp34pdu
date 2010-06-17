{application, smpp34_pdu,
 [
  {description, ""},
  {vsn, "1"},
  {modules, [
	  		pdu_data,
			smpp34_BIND_RECEIVER
            ]},
  {registered, []},
  {applications, [
                  kernel,
                  stdlib
                 ]},
  {env, []}
 ]}.
