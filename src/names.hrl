-ifndef(names).
-define(names, true).

-define(SMPP_VERSION(Version),
    case Version of
        16#30 -> 3.0;
        16#33 -> 3.3;
        16#34 -> 3.4;
        16#50 -> 5.0;
        N -> N
    end).

-define(SMPP_STATUS(Status), 
    case Status of
        ?ESME_ROK -> 'ESME_ROK';
        ?ESME_RINVMSGLEN -> 'ESME_RINVMSGLEN';
        ?ESME_RINVCMDLEN -> 'ESME_RINVCMDLEN';
        ?ESME_RINVCMDID -> 'ESME_RINVCMDID';
        ?ESME_RINVBNDSTS -> 'ESME_RINVBNDSTS';
        ?ESME_RALYBND -> 'ESME_RALYBND';
        ?ESME_RINVPRTFLG -> 'ESME_RINVPRTFLG';
        ?ESME_RINVREGDLVFLG -> 'ESME_RINVREGDLVFLG';
        ?ESME_RSYSERR -> 'ESME_RSYSERR';
        ?ESME_RINVSRCADR -> 'ESME_RINVSRCADR';
        ?ESME_RINVDSTADR -> 'ESME_RINVDSTADR';
        ?ESME_RINVMSGID -> 'ESME_RINVMSGID';
        ?ESME_RBINDFAIL -> 'ESME_RBINDFAIL';
        ?ESME_RINVPASWD -> 'ESME_RINVPASWD';
        ?ESME_RINVSYSID -> 'ESME_RINVSYSID';
        ?ESME_RCANCELFAIL -> 'ESME_RCANCELFAIL';
        ?ESME_RREPLACEFAIL -> 'ESME_RREPLACEFAIL';
        ?ESME_RMSGQFUL -> 'ESME_RMSGQFUL';
        ?ESME_RINVSERTYP -> 'ESME_RINVSERTYP';
        ?ESME_RINVNUMDESTS -> 'ESME_RINVNUMDESTS';
        ?ESME_RINVDLNAME -> 'ESME_RINVDLNAME';
        ?ESME_RINVDESTFLAG -> 'ESME_RINVDESTFLAG';
        ?ESME_RINVSUBREP -> 'ESME_RINVSUBREP';
        ?ESME_RINVESMCLASS -> 'ESME_RINVESMCLASS';
        ?ESME_RCNTSUBDL -> 'ESME_RCNTSUBDL';
        ?ESME_RSUBMITFAIL -> 'ESME_RSUBMITFAIL';
        ?ESME_RINVSRCTON -> 'ESME_RINVSRCTON';
        ?ESME_RINVSRCNPI -> 'ESME_RINVSRCNPI';
        ?ESME_RINVDSTTON -> 'ESME_RINVDSTTON';
        ?ESME_RINVDSTNPI -> 'ESME_RINVDSTNPI';
        ?ESME_RINVSYSTYP -> 'ESME_RINVSYSTYP';
        ?ESME_RINVREPFLAG -> 'ESME_RINVREPFLAG';
        ?ESME_RINVNUMMSGS -> 'ESME_RINVNUMMSGS';
        ?ESME_RTHROTTLED -> 'ESME_RTHROTTLED';
        ?ESME_RINVSCHED -> 'ESME_RINVSCHED';
        ?ESME_RINVEXPIRY -> 'ESME_RINVEXPIRY';
        ?ESME_RINVDFTMSGID -> 'ESME_RINVDFTMSGID';
        ?ESME_RX_T_APPN -> 'ESME_RX_T_APPN';
        ?ESME_RX_P_APPN -> 'ESME_RX_P_APPN';
        ?ESME_RX_R_APPN -> 'ESME_RX_R_APPN';
        ?ESME_RQUERYFAIL -> 'ESME_RQUERYFAIL';
        ?ESME_RINVOPTPARSTREAM -> 'ESME_INVOPTPARSTREAM';
        ?ESME_ROPTPARNOTALLWD -> 'ESME_ROPTPARNOTALLWD';
        ?ESME_RINVPARLEN -> 'ESME_RINVPARLEN';
        ?ESME_RMISSINGOPTPARAM -> 'ESME_RMISSINGOPTPARAM';
        ?ESME_RINVOPTPARAMVAL -> 'ESME_RINVOPTPARAMVAL';
        ?ESME_RDELIVERYFAILURE -> 'ESME_RDELIVERYFAILURE';
        ?ESME_RUNKNOWNERR -> 'ESME_RUNKNOWNERR';
        _-> 'INVALID_SMPP_STATUS'
   end).

-define(SMPP_PDU2CMDID(PduBody),
    case PduBody of
        #generic_nack{} -> 'GENERIC_NACK';
        #bind_receiver{} -> 'BIND_RECEIVER';
        #bind_receiver_resp{} -> 'BIND_RECEIVER_RESP';
        #bind_transmitter{} -> 'BIND_TRANSMITTER';
        #bind_transmitter_resp{} -> 'BIND_TRANSMITTER_RESP';
        #query_sm{} -> 'QUERY_SM';
        #query_sm_resp{} -> 'QUERY_SM_RESP';
        #submit_sm{} -> 'SUBMIT_SM';
        #submit_sm_resp{} -> 'SUBMIT_SM_RESP';
        #deliver_sm{} -> 'DELIVER_SM';
        #deliver_sm_resp{} -> 'DELIVER_SM_RESP';
        #unbind{} -> 'UNBIND';
        #unbind_resp{} -> 'UNBIND_RESP';
        #replace_sm{} -> 'REPLACE_SM';
        #replace_sm_resp{} -> 'REPLACE_SM_RESP';
        #cancel_sm{} -> 'CANCEL_SM';
        #cancel_sm_resp{} -> 'CANCEL_SM_RESP';
        #bind_transceiver{} -> 'BIND_TRANSCEIVER';
        #bind_transceiver_resp{} -> 'BIND_TRANSCEIVER_RESP';
        #outbind{} -> 'OUTBIND';
        #enquire_link{} -> 'ENQUIRE_LINK';
        #enquire_link_resp{} -> 'ENQUIRE_LINK_RESP';
   %     #submit_multi{} -> 'SUBMIT_MULTI';
   %     #submit_multi_resp{} -> 'SUBMIT_MULTI_RESP';
        #alert_notification{} -> 'ALERT_NOTIFICATION';
   %%     #data_sm{} -> 'DATA_SM';
        #data_sm_resp{} -> 'DATA_SM_RESP'
    end).

-endif.
