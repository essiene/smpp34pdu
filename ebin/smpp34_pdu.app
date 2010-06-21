{application, smpp34_pdu,
 [
  {description, ""},
  {vsn, "1"},
  {modules, [
	  		pdu_data,
			smpp34_pdu,
			smpp34_BIND_RECEIVER,
			smpp34_BIND_TRANSMITTER
            ]},
  {registered, []},
  {applications, [
                  kernel,
                  stdlib
                 ]},
  {env, []}
 ]}.
