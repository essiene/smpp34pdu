{application, smpp34pdu,
 [
  {description, ""},
  {vsn, "1"},
  {modules, [
	  		pdu_data,
			smpp34pdu,
			smpp34pdu_bind_receiver,
			smpp34pdu_bind_transmitter,
			smpp34pdu_bind_transceiver,
			smpp34pdu_unbind,
			smpp34pdu_unbind_resp,
			smpp34pdu_enquire_link,
			smpp34pdu_enquire_link_resp,
			smpp34pdu_replace_sm_resp,
			smpp34pdu_cancel_sm_resp,
			smpp34pdu_generic_nack
            ]},
  {registered, []},
  {applications, [
                  kernel,
                  stdlib
                 ]},
  {env, []}
 ]}.
