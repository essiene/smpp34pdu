{application, smpp34pdu,
 [
  {description, ""},
  {vsn, "1"},
  {modules, [
	  		pdu_data,
			smpp34pdu,
			smpp34pdu_bind_receiver,
			smpp34pdu_bind_transmitter,
			smpp34pdu_bind_transceiver
            ]},
  {registered, []},
  {applications, [
                  kernel,
                  stdlib
                 ]},
  {env, []}
 ]}.
