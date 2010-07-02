{application, smpp34pdu,
 [
  {description, ""},
  {vsn, "1"},
  {modules, [
	  		pdu_data,
			smpp34pdu,
			smpp34pdu_bind_receiver,
			smpp34pdu_bind_transmitter,
			smpp34pdu_query_sm,
			smpp34pdu_query_sm_resp,
			smpp34pdu_submit_sm_resp,
			smpp34pdu_deliver_sm_resp,
			smpp34pdu_replace_sm,
			smpp34pdu_cancel_sm,
			smpp34pdu_bind_transceiver,
			smpp34pdu_outbind
            ]},
  {registered, []},
  {applications, [
                  kernel,
                  stdlib
                 ]},
  {env, []}
 ]}.
