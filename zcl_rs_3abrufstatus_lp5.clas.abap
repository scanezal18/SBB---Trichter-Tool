class ZCL_RS_3ABRUFSTATUS_LP5 definition
  public
  final
  create public .

public section.

  interfaces ZIF_RS_3DPC_ABRUFSTATUS .

  class-methods CREATE
    importing
      !IS_PARAMS type zif_rs_3dpc_abrufstatus=>TS_INPUT_PARAMETERS
    returning
      value(RV_OBJECT) type ref to ZIF_RS_3DPC_ABRUFSTATUS
    raising
      /IWBEP/CX_MGW_TECH_EXCEPTION
      /IWBEP/CX_MGW_BUSI_EXCEPTION .
  methods CONSTRUCTOR
    importing
      !IS_PARAMS type zif_rs_3dpc_abrufstatus=>TS_INPUT_PARAMETERS
    raising
      /IWBEP/CX_MGW_TECH_EXCEPTION
      /IWBEP/CX_MGW_BUSI_EXCEPTION .
  PROTECTED SECTION.
private section.

  data GS_PARAMS type ZIF_RS_3DPC_ABRUFSTATUS=>TS_INPUT_PARAMETERS .
  constants:
    BEGIN OF mc_error_opt,
      business  TYPE char1 VALUE 'B',
      technical TYPE char1 VALUE 'T',
    END OF mc_error_opt .

  methods GET_BILLING .
  methods GET_KOPF_OLD .
  methods GET_POS_OLD .
  methods GET_VORHABEN .
  methods PROCESS_STATUS_CHANGE .
  methods SET_EMAIL .
  methods RAISE_ERROR
    importing
      !IS_OPT type CHAR1 default MC_ERROR_OPT-BUSINESS
      !IS_TEXTID like IF_T100_MESSAGE=>T100KEY default /IWBEP/CX_MGW_BUSI_EXCEPTION=>BUSINESS_ERROR
      !IS_MESSAGE type STRING optional
      !IS_METHODNAME type STRING optional
      !IS_OMESSAGECONTAINER type ref to /IWBEP/IF_MESSAGE_CONTAINER optional
    raising
      /IWBEP/CX_MGW_TECH_EXCEPTION
      /IWBEP/CX_MGW_BUSI_EXCEPTION .
ENDCLASS.



CLASS ZCL_RS_3ABRUFSTATUS_LP5 IMPLEMENTATION.


  METHOD CONSTRUCTOR.
    gs_params = is_params.
  ENDMETHOD.


  METHOD CREATE.
    DATA(lo_status) = NEW zcl_rs_3abrufstatus_lp5( is_params ).
    rv_object = lo_status.
  ENDMETHOD.


  METHOD get_billing.

    DATA: ls_verrchng TYPE zrs_3tt_verrchng.
    DATA(lv_abrufnummer) = gs_params-er_entity_pos-abrufnummer.
    DATA(lv_abrufposition) = gs_params-er_entity_pos-abrufposition.

    SELECT SINGLE *
      FROM zrs_3tt_verrchng
      INTO ls_verrchng
      WHERE abrufnummer EQ lv_abrufnummer
        AND abrufposition EQ lv_abrufposition.
    IF sy-subrc EQ 0.
      MOVE-CORRESPONDING ls_verrchng TO gs_params-er_entity_billing.
    ENDIF.

  ENDMETHOD.


  METHOD get_kopf_old.

    DATA: ls_abrufkop TYPE zrs_3tt_abrufkop.
    DATA(lv_abrufnummer) = gs_params-er_entity_pos-abrufnummer.

    SELECT SINGLE *
      FROM zrs_3tt_abrufkop
      INTO ls_abrufkop
      WHERE abrufnummer EQ lv_abrufnummer.
    IF sy-subrc EQ 0.
      MOVE-CORRESPONDING ls_abrufkop TO gs_params-er_entity_kopf_old.
    ENDIF.

  ENDMETHOD.


  method GET_POS_OLD.

    DATA: ls_abrufpos TYPE zrs_3tt_abrufpos.
    DATA(lv_abrufnummer) = gs_params-er_entity_pos-abrufnummer.

    SELECT SINGLE *
      FROM zrs_3tt_abrufpos
      INTO ls_abrufpos
      WHERE abrufnummer EQ lv_abrufnummer.
    IF sy-subrc EQ 0.
      MOVE-CORRESPONDING ls_abrufpos TO gs_params-er_entity_pos_old.
    ENDIF.

  endmethod.


  METHOD get_vorhaben.

    SELECT SINGLE *
      FROM zrs_3tt_vorhaben
      INTO @DATA(ls_vorhaben)
      WHERE vorhabensnummer EQ @gs_params-er_entity_kopf_old-vorhabensnummer.
    IF sy-subrc EQ 0.
      MOVE-CORRESPONDING ls_vorhaben TO gs_params-er_entity_vorhaben.
    ENDIF.

  ENDMETHOD.


  METHOD process_status_change.

    DATA: lv_set15 TYPE flag.
    DATA(lv_abrufkop_old) = gs_params-er_entity_kopf_old.
    DATA(lv_abrufkop) = gs_params-er_entity_kopf.
    DATA(lv_abrufpos_old) = gs_params-er_entity_pos_old.
    DATA(lv_abrufpos) = gs_params-er_entity_pos.

    IF lv_abrufpos_old-abnahme_status EQ '010' "New
      AND lv_abrufpos-abnahme_status EQ '210'.  "sprint called

      lv_abrufpos-abrufer = sy-uname.
      lv_abrufpos-abrufdatum = sy-datum.
      "calculate cost_position

      "check if all positions of the call is in 210
      SELECT *
        FROM zrs_3tt_abrufpos
        INTO TABLE @DATA(lv_pos_all)
        WHERE abrufnummer EQ @lv_abrufpos-abrufnummer.
      IF sy-subrc EQ 0.
        DELETE lv_pos_all
          WHERE abrufposition EQ lv_abrufpos-abrufposition.
        IF lv_pos_all[] IS NOT INITIAL.
          LOOP AT lv_pos_all INTO DATA(ls_pos_all).
            IF ls_pos_all-abnahme_status EQ '210'.
              lv_set15 = abap_true.
            ELSE.
              CLEAR: lv_set15.
            ENDIF.
          ENDLOOP.
        ELSE.
          lv_set15 = abap_true.
        ENDIF.
      ENDIF.

      IF lv_set15 IS NOT INITIAL.
        "set header status to 015
        lv_abrufkop = lv_abrufkop_old.
        lv_abrufkop-status_abruf = '015'. "in process
        lv_abrufkop-abgerufen_am = sy-datum.
      ELSE.
        CLEAR: lv_abrufkop.
      ENDIF.


      "update header and pos table
      gs_params-er_entity_kopf = lv_abrufkop.
      gs_params-er_entity_pos = lv_abrufpos.


      "setup email id and recipients
      DATA(lo_vorhaben) = NEW zcl_rs_3vorhaben( ).

*      gs_params-es_email_params-iv_mail_id = 'ZRS_TRICHTER_lp5id'.
      gs_params-es_email_params-it_recipients = VALUE #(
        (
          address = lo_vorhaben->get_email_address( EXPORTING iv_userid =  gs_params-er_entity_vorhaben-service_spoc )
          address_type = 'U'
          recepient_type = 'A'
        )
        (
          address = lo_vorhaben->get_email_address( EXPORTING iv_userid =  gs_params-er_entity_vorhaben-stv_sspoc )
          address_type = 'U'
          recepient_type = 'C'
        )
      ).

      DELETE gs_params-es_email_params-it_recipients
         WHERE address = space.

      gs_params-es_email_params-it_name_value = VALUE #(
        (
          name = 'ID'
          value = gs_params-er_entity_kopf-vorhabensnummer
        )
        (
          name = 'DESC'
          value = gs_params-er_entity_vorhaben-projektbezeichnung
        )
        (
          name = 'ABRUF'
          value = gs_params-er_entity_pos-abrufnummer
        )
      ).

    ELSEIF lv_abrufpos_old-abnahme_status EQ '210' "sprint called
      AND ( lv_abrufpos-abnahme_status EQ '211'  "delivered and accepted
            OR lv_abrufpos-abnahme_status EQ '212' ).  "not delivered

      lv_abrufpos-realisiertes_lieferdatum = sy-datum.
      gs_params-er_entity_pos = lv_abrufpos.

      "create billing table entry only for 211
      IF lv_abrufpos-abnahme_status EQ '211'.
        gs_params-er_entity_billing = VALUE #( abrufnummer = gs_params-er_entity_pos-abrufnummer
                                               abrufposition = gs_params-er_entity_pos-abrufposition
                                               lieferanteil = zif_rs_3constants=>gc_katlieft_status-story"'STORY'
                                               lieferdatum = sy-datum
                                               abnahmedatum = sy-datum
                                               wert_position = gs_params-er_entity_pos-wert_position
                                               kosten_position = gs_params-er_entity_pos-kosten_position
                                               einheit_wert = gs_params-er_entity_pos-einheit_wert
                                               waehrung = gs_params-er_entity_pos-waehrung ).

        DATA lv_params TYPE zcl_rs_3abrufpos=>ts_input_parameters.
        DATA(lv_anteil) = NEW zcl_rs_3abrufpos( lv_params )->get_katlief_anteil( gs_params-er_entity_billing-lieferanteil ).
        IF lv_anteil IS NOT INITIAL.
          gs_params-er_entity_billing-kosten_anteil = gs_params-er_entity_pos-kosten_position * ( lv_anteil / 100 ).
          gs_params-er_entity_billing-wert_pos_anteil = gs_params-er_entity_pos-wert_position * ( lv_anteil / 100 ).
        ENDIF.
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD raise_error.
*----------------------------------------------------------------------*
* SBB AG, SAP CCC-IT, Bern
*----------------------------------------------------------------------*
*
*-------------Aenderungsdokumentation----------------------------------*
* Edition SAP-Rel    Datum        Bearbeiter
*         Beschreibung
*----------------------------------------------------------------------*
* >000<   7.50       28.10.2020   DXC / Sigried Canezal
*         Trichtertool - Program Development
*----------------------------------------------------------------------*
* >001<   7.50       tt.mm.jjjj   Firma / Name
*         ...
*----------------------------------------------------------------------*
    DATA(lv_methodname) = is_methodname.
    DATA(lv_message) = CONV bapi_msg( is_message ).
    DATA(lo_messagecontainer) = is_omessagecontainer.

    CASE is_opt.
      WHEN mc_error_opt-business.
        IF lv_message IS NOT INITIAL.
          RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
            EXPORTING
              textid  = is_textid
              message = lv_message.
        ENDIF.
        IF lo_messagecontainer IS BOUND.
          RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
            EXPORTING
              textid            = is_textid
              message_container = lo_messagecontainer.
        ENDIF.
      WHEN mc_error_opt-technical.
        IF lv_methodname IS NOT INITIAL.
          RAISE EXCEPTION TYPE /iwbep/cx_mgw_tech_exception
            EXPORTING
              textid = is_textid
              method = lv_methodname.
        ENDIF.
      WHEN OTHERS.
        RETURN.
    ENDCASE.
  ENDMETHOD.


  METHOD set_email.

    DATA(lo_vorhaben) = NEW zcl_rs_3vorhaben( ).
    gs_params-es_email_params-iv_sender_address = lo_vorhaben->get_email_address( EXPORTING iv_userid = sy-uname ).
    gs_params-es_email_params-iv_sender_address_type = 'U'.
    gs_params-es_email_params-iv_language            = sy-langu.
    gs_params-es_email_params-iv_send_immediately    = 'X'.
    gs_params-es_email_params-iv_html                = 'X'.
    gs_params-es_email_params-iv_subject_as_title    = ''.

  ENDMETHOD.


  method ZIF_RS_3DPC_ABRUFSTATUS~CHECK.

    DATA(lv_uname) = sy-uname.

    get_kopf_old( ).
    get_pos_old( ).
    get_billing( ).
    get_vorhaben( ).
    set_email( ).

    process_status_change( ).

*    check if allowed to edit/locking validation
*    IF gs_params-is_cud_type = zif_rs_3constants=>gc_cud_types-update.
*      IF gs_params-er_entity_pos_old-lastchangedon <> gs_params-er_entity_pos-lastchangedon.
*        raise_error( is_message = 'Daten werden derzeit geändert. Bitte versuchen Sie es später noch einmal.' ). "Data is currently being modified. Please try again later.
*      ENDIF.
*    ENDIF.

*   set timestamp
    DATA lv_timestamp TYPE timestamp.
    GET TIME STAMP FIELD lv_timestamp.
    DATA(lv_secs) = zif_rs_3constants=>gc_session_expires_secs-position * zif_rs_3constants=>gc_session_expires_secs-multiplier.

    lv_timestamp = cl_abap_tstmp=>subtractsecs( tstmp  = lv_timestamp
                                                secs = lv_secs ).
*    set changedlogs
    gs_params-er_entity_pos-lastchangedby = lv_uname.
    gs_params-er_entity_pos-lastchangedon = lv_timestamp.
  endmethod.


  method ZIF_RS_3DPC_ABRUFSTATUS~GET_RESULTS.
    es_entity_vorhaben = gs_params-er_entity_vorhaben.
    es_entity_kopf = gs_params-er_entity_kopf.
    es_entity_pos = gs_params-er_entity_pos.
    es_entity_billing = gs_params-er_entity_billing.
    es_entity_kopf_old = gs_params-er_entity_kopf_old.
    es_entity_pos_old = gs_params-er_entity_pos_old.
    es_email_params = gs_params-es_email_params.
    es_mo_context = gs_params-es_mo_context.
  endmethod.
ENDCLASS.
