class ZCL_RS_3ABRUFSTATUS_LP3 definition
  public
  final
  create public .

public section.

  interfaces ZIF_RS_3DPC_ABRUFSTATUS .

  class-methods CREATE
    importing
      !IS_PARAMS type ZIF_RS_3DPC_ABRUFSTATUS=>TS_INPUT_PARAMETERS
    returning
      value(RV_OBJECT) type ref to ZIF_RS_3DPC_ABRUFSTATUS
    raising
      /IWBEP/CX_MGW_TECH_EXCEPTION
      /IWBEP/CX_MGW_BUSI_EXCEPTION .
  methods CONSTRUCTOR
    importing
      !IS_PARAMS type ZIF_RS_3DPC_ABRUFSTATUS=>TS_INPUT_PARAMETERS
    raising
      /IWBEP/CX_MGW_TECH_EXCEPTION
      /IWBEP/CX_MGW_BUSI_EXCEPTION .
  methods GET_PROJECT
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
  data:
    GS_ABRUF_POS TYPE STANDARD TABLE OF ZRS_3TT_ABRUFPOS .

  methods GET_KOPF
    raising
      /IWBEP/CX_MGW_TECH_EXCEPTION
      /IWBEP/CX_MGW_BUSI_EXCEPTION .
  methods GET_ALL_POS
    raising
      /IWBEP/CX_MGW_TECH_EXCEPTION
      /IWBEP/CX_MGW_BUSI_EXCEPTION .
  methods GET_BILLING
    raising
      /IWBEP/CX_MGW_TECH_EXCEPTION
      /IWBEP/CX_MGW_BUSI_EXCEPTION .
  methods SET_DEFAULT_EMAIL_CONFIG .
  methods VALIDATE
    raising
      /IWBEP/CX_MGW_TECH_EXCEPTION
      /IWBEP/CX_MGW_BUSI_EXCEPTION .
  methods CHECK_10_TO_110
    raising
      /IWBEP/CX_MGW_TECH_EXCEPTION
      /IWBEP/CX_MGW_BUSI_EXCEPTION .
  methods CHECK_110_TO_10 .
  methods CHECK_110_TO_111
    raising
      /IWBEP/CX_MGW_TECH_EXCEPTION
      /IWBEP/CX_MGW_BUSI_EXCEPTION .
  methods CHECK_111_TO_900
    raising
      /IWBEP/CX_MGW_TECH_EXCEPTION
      /IWBEP/CX_MGW_BUSI_EXCEPTION .
  methods CHECK_111_TO_112
    raising
      /IWBEP/CX_MGW_TECH_EXCEPTION
      /IWBEP/CX_MGW_BUSI_EXCEPTION .
  methods CHECK_112_TO_113
    raising
      /IWBEP/CX_MGW_TECH_EXCEPTION
      /IWBEP/CX_MGW_BUSI_EXCEPTION .
  methods CHECK_113_TO_114
    raising
      /IWBEP/CX_MGW_TECH_EXCEPTION
      /IWBEP/CX_MGW_BUSI_EXCEPTION .
  methods RAISE_ERROR
    importing
      !IS_OPT type CHAR1 default MC_ERROR_OPT-BUSINESS
      !IS_TEXTID like IF_T100_MESSAGE=>T100KEY default /IWBEP/CX_MGW_BUSI_EXCEPTION=>BUSINESS_ERROR
      !IS_MESSAGE type STRING optional
      !IS_METHODNAME type STRING optional
    raising
      /IWBEP/CX_MGW_TECH_EXCEPTION
      /IWBEP/CX_MGW_BUSI_EXCEPTION .
ENDCLASS.



CLASS ZCL_RS_3ABRUFSTATUS_LP3 IMPLEMENTATION.


  METHOD check_10_to_110.
*----------------------------------------------------------------------*
* SBB AG, SAP CCC-IT, Bern/Berne/Berna
*----------------------------------------------------------------------*
*
*-----------Änderungsdokumentation / documentation of changes----------*
* Version SAP-Rel Datum Bearbeiter
* Beschreibung Validate Status New to Testplan for Checking
*----------------------------------------------------------------------*
* >000< 7.50 28.10.2020 Firma(DXC Technology) / Name(Shernan Bamba)
* Programmerstellung / creation of program
*----------------------------------------------------------------------*
* >001< 7.50 tt.mm.jjjj Firma(company) / Name(name)
* ...
*----------------------------------------------------------------------*

    IF gs_params-er_entity_pos-abrufnummer    IS NOT INITIAL
    AND gs_params-er_entity_pos-abrufposition IS NOT INITIAL
    AND gs_params-er_entity_pos-artikelnummer IS NOT INITIAL
    AND gs_params-er_entity_pos-menge         IS NOT INITIAL
    AND gs_params-er_entity_pos-wert_objekt   IS NOT INITIAL
    AND gs_params-er_entity_pos-einheit_wert  IS NOT INITIAL
    AND gs_params-er_entity_pos-wert_position IS NOT INITIAL
    AND gs_params-er_entity_pos-referenz      IS NOT INITIAL.

*"Send email here: ZRS_TRICHTER_30001
*"Recipient: gs_params-er_entity_vorhaben-Service_spoc
*"CC: gs_params-er_entity_vorhaben-STV_SSPOC
*"Name/Value: ID = gs_params-er_entity_kopf-vorhabensnummer
*"DESC = gs_params-er_entity_vorhaben-PROJEKTBEZEICHNUNG
*"ABRUF = gs_params-er_entity_pos-abrufnummer

      DATA(lo_vorhaben) = NEW zcl_rs_3vorhaben( ).

      gs_params-es_email_params-iv_mail_id = zif_rs_3constants=>gc_email_templates_abruf-et310110. "ZRS_TRICHTER_30001

      gs_params-es_email_params-it_recipients = VALUE #(
        (
          address = lo_vorhaben->get_email_address( EXPORTING iv_userid =  gs_params-er_entity_vorhaben-service_spoc )
          address_type = zif_rs_3constants=>gc_email_kind-address_type_u
          recepient_type = zif_rs_3constants=>gc_email_kind-recepient_type_to
        )
        (
          address = lo_vorhaben->get_email_address( EXPORTING iv_userid =  gs_params-er_entity_vorhaben-stv_sspoc )
          address_type = zif_rs_3constants=>gc_email_kind-address_type_u
          recepient_type = zif_rs_3constants=>gc_email_kind-recepient_type_cc
        )
      ).

      DELETE gs_params-es_email_params-it_recipients
         WHERE address = space.

      gs_params-es_email_params-it_name_value = VALUE #(
        (
          name = zif_rs_3constants=>gc_email_namevalues_abruf-vorhabensnummer
          value = gs_params-er_entity_kopf-vorhabensnummer
        )
        (
          name = zif_rs_3constants=>gc_email_namevalues_abruf-projektbezeichnung
          value = gs_params-er_entity_vorhaben-projektbezeichnung
        )
        (
          name = zif_rs_3constants=>gc_email_namevalues_abruf-abrufnummer
          value = gs_params-er_entity_pos-abrufnummer
        )
      ).

    ELSE.

      MESSAGE e037(zrs_3) INTO DATA(lv_message).
      raise_error( is_message = lv_message ).

    ENDIF.


  ENDMETHOD.


  METHOD check_110_to_10.
*----------------------------------------------------------------------*
* SBB AG, SAP CCC-IT, Bern/Berne/Berna
*----------------------------------------------------------------------*
*
*-----------Änderungsdokumentation / documentation of changes----------*
* Version SAP-Rel Datum Bearbeiter
* Beschreibung Validate status Testplan for checking to New
*----------------------------------------------------------------------*
* >000< 7.50 28.10.2020 Firma(DXC Technology) / Name(Shernan Bamba)
* Programmerstellung / creation of program
*----------------------------------------------------------------------*
* >001< 7.50 tt.mm.jjjj Firma(company) / Name(name)
* ...
*----------------------------------------------------------------------*

    IF gs_params-er_entity_pos-notiz IS NOT INITIAL.

*"Send email here: ZRS_TRICHTER_30003
*"Recipient: gs_params-er_entity_vorhaben-projektleiter
*"CC: gs_params-er_entity_vorhaben-Stv_projekleiter
*"Name/Value: ID = gs_params-er_entity_kopf-vorhabensnummer
*"            DESC = gs_params-er_entity_vorhaben-PROJEKTBEZEICHNUNG
*"            ABRUF = gs_params-er_entity_pos-abrufnummer
*"            NOTIZ = gs_params-er_entity_pos-notiz

      DATA(lo_vorhaben) = NEW zcl_rs_3vorhaben( ).

      gs_params-es_email_params-iv_mail_id = zif_rs_3constants=>gc_email_templates_abruf-et311010. "ZRS_TRICHTER_30003

      gs_params-es_email_params-it_recipients = VALUE #(
        (
          address = lo_vorhaben->get_email_address( EXPORTING iv_userid =  gs_params-er_entity_vorhaben-projektleiter )
          address_type = zif_rs_3constants=>gc_email_kind-address_type_u
          recepient_type = zif_rs_3constants=>gc_email_kind-recepient_type_to
        )
        (
          address = lo_vorhaben->get_email_address( EXPORTING iv_userid =  gs_params-er_entity_vorhaben-stv_projektleiter )
          address_type = zif_rs_3constants=>gc_email_kind-address_type_u
          recepient_type = zif_rs_3constants=>gc_email_kind-recepient_type_cc
        )
      ).

      DELETE gs_params-es_email_params-it_recipients
         WHERE address = space.

      gs_params-es_email_params-it_name_value = VALUE #(
        (
          name = zif_rs_3constants=>gc_email_namevalues_abruf-vorhabensnummer
          value = gs_params-er_entity_kopf-vorhabensnummer
        )
        (
          name = zif_rs_3constants=>gc_email_namevalues_abruf-projektbezeichnung
          value = gs_params-er_entity_vorhaben-projektbezeichnung
        )
        (
          name = zif_rs_3constants=>gc_email_namevalues_abruf-abrufnummer
          value = gs_params-er_entity_pos-abrufnummer
        )
        (
          name = zif_rs_3constants=>gc_email_namevalues_abruf-notiz
          value = gs_params-er_entity_pos-notiz
        )
      ).

    ELSE.

*      MESSAGE e037(zrs_3) INTO DATA(lv_message). "Note is initial
*      raise_error( is_message = lv_message ).

    ENDIF.

  ENDMETHOD.


  METHOD check_110_to_111.
*----------------------------------------------------------------------*
* SBB AG, SAP CCC-IT, Bern/Berne/Berna
*----------------------------------------------------------------------*
*
*-----------Änderungsdokumentation / documentation of changes----------*
* Version SAP-Rel Datum Bearbeiter
* Beschreibung Validate status Testplan for checking to Testplan Ready
*----------------------------------------------------------------------*
* >000< 7.50 28.10.2020 Firma(DXC Technology) / Name(Shernan Bamba)
* Programmerstellung / creation of program
*----------------------------------------------------------------------*
* >001< 7.50 tt.mm.jjjj Firma(company) / Name(name)
* ...
*----------------------------------------------------------------------*

*"Send email here: ZRS_TRICHTER_30004
*"Recipient: gs_params-er_entity_vorhaben-projektleiter
*"CC: gs_params-er_entity_vorhaben-Stv_projekleiter
*"Name/Value: ID = gs_params-er_entity_kopf-vorhabensnummer
*"            DESC = gs_params-er_entity_vorhaben-PROJEKTBEZEICHNUNG
*"            ABRUF = gs_params-er_entity_pos-abrufnummer

    DATA(lo_vorhaben) = NEW zcl_rs_3vorhaben( ).

    gs_params-es_email_params-iv_mail_id = zif_rs_3constants=>gc_email_templates_abruf-et3110111. "ZRS_TRICHTER_30004

    gs_params-es_email_params-it_recipients = VALUE #(
      (
        address = lo_vorhaben->get_email_address( EXPORTING iv_userid =  gs_params-er_entity_vorhaben-projektleiter )
        address_type = zif_rs_3constants=>gc_email_kind-address_type_u
        recepient_type = zif_rs_3constants=>gc_email_kind-recepient_type_to
      )
      (
        address = lo_vorhaben->get_email_address( EXPORTING iv_userid =  gs_params-er_entity_vorhaben-stv_projektleiter )
        address_type = zif_rs_3constants=>gc_email_kind-address_type_u
        recepient_type = zif_rs_3constants=>gc_email_kind-recepient_type_cc
      )
    ).

    DELETE gs_params-es_email_params-it_recipients
       WHERE address = space.

    gs_params-es_email_params-it_name_value = VALUE #(
      (
        name = zif_rs_3constants=>gc_email_namevalues_abruf-vorhabensnummer
        value = gs_params-er_entity_kopf-vorhabensnummer
      )
      (
        name = zif_rs_3constants=>gc_email_namevalues_abruf-projektbezeichnung
        value = gs_params-er_entity_vorhaben-projektbezeichnung
      )
      (
        name = zif_rs_3constants=>gc_email_namevalues_abruf-abrufnummer
        value = gs_params-er_entity_pos-abrufnummer
      )
    ).

  ENDMETHOD.


  METHOD check_111_to_112.
*----------------------------------------------------------------------*
* SBB AG, SAP CCC-IT, Bern/Berne/Berna
*----------------------------------------------------------------------*
*
*-----------Änderungsdokumentation / documentation of changes----------*
* Version SAP-Rel Datum Bearbeiter
* Beschreibung Validate status Testplan Ready to Testplan Retrieved
*----------------------------------------------------------------------*
* >000< 7.50 28.10.2020 Firma(DXC Technology) / Name(Shernan Bamba)
* Programmerstellung / creation of program
*----------------------------------------------------------------------*
* >001< 7.50 tt.mm.jjjj Firma(company) / Name(name)
* ...
*----------------------------------------------------------------------*
*When the request is made, the field Cost_Position is recalculated with the currently valid key date price for the unit of measure used, value_unit. see FUNNEL-52
*Trichter-52: Cost_Position = Value_Position multiplied by the current price (price table with the selections: reference date current calendar day & unit equal to value_unit of the position)
*Calculation done and passed in UI

    IF gs_params-er_entity_pos-kosten_position IS INITIAL.

      MESSAGE e037(zrs_3) INTO DATA(lv_message). "Cost Position is initial
      raise_error( is_message = lv_message ).

    ENDIF.
*The request date is set to the current date in the header and position and the current user is logged in the field requestor.
    gs_params-er_entity_pos-abrufer = sy-uname.
    gs_params-er_entity_pos-abrufdatum = sy-datum.

*After saving, all fields are blocked for changes except for the note field on the header and position

*"Send email here: ZRS_TRICHTER_30005
*"Recipient: gs_params-er_entity_vorhaben-service_spoc
*"CC: gs_params-er_entity_vorhaben-Stv_sspoc
*"Name/Value: ID = gs_params-er_entity_kopf-vorhabensnummer
*"            DESC = gs_params-er_entity_vorhaben-PROJEKTBEZEICHNUNG
*"            ABRUF = gs_params-er_entity_pos-abrufnummer

    DATA(lo_vorhaben) = NEW zcl_rs_3vorhaben( ).

    gs_params-es_email_params-iv_mail_id = zif_rs_3constants=>gc_email_templates_abruf-et3111112. "ZRS_TRICHTER_30005

    gs_params-es_email_params-it_recipients = VALUE #(
      (
        address = lo_vorhaben->get_email_address( EXPORTING iv_userid =  gs_params-er_entity_vorhaben-service_spoc )
        address_type = zif_rs_3constants=>gc_email_kind-address_type_u
        recepient_type = zif_rs_3constants=>gc_email_kind-recepient_type_to
      )
      (
        address = lo_vorhaben->get_email_address( EXPORTING iv_userid =  gs_params-er_entity_vorhaben-stv_sspoc )
        address_type = zif_rs_3constants=>gc_email_kind-address_type_u
        recepient_type = zif_rs_3constants=>gc_email_kind-recepient_type_cc
      )
    ).

    DELETE gs_params-es_email_params-it_recipients
       WHERE address = space.

    gs_params-es_email_params-it_name_value = VALUE #(
      (
        name = zif_rs_3constants=>gc_email_namevalues_abruf-vorhabensnummer
        value = gs_params-er_entity_kopf-vorhabensnummer
      )
      (
        name = zif_rs_3constants=>gc_email_namevalues_abruf-projektbezeichnung
        value = gs_params-er_entity_vorhaben-projektbezeichnung
      )
      (
        name = zif_rs_3constants=>gc_email_namevalues_abruf-abrufnummer
        value = gs_params-er_entity_pos-abrufnummer
      )
    ).

  ENDMETHOD.


  METHOD check_111_to_900.
*----------------------------------------------------------------------*
* SBB AG, SAP CCC-IT, Bern/Berne/Berna
*----------------------------------------------------------------------*
*
*-----------Änderungsdokumentation / documentation of changes----------*
* Version SAP-Rel Datum Bearbeiter
* Beschreibung Validate status Testplan for checking to New
*----------------------------------------------------------------------*
* >000< 7.50 28.10.2020 Firma(DXC Technology) / Name(Shernan Bamba)
* Programmerstellung / creation of program
*----------------------------------------------------------------------*
* >001< 7.50 tt.mm.jjjj Firma(company) / Name(name)
* ...
*----------------------------------------------------------------------*
*"If all pos under header is cancelled, cancel header status as well
    FREE: gs_params-es_email_params.

    get_all_pos( ).

    DELETE gs_abruf_pos
      WHERE abrufposition = gs_params-er_entity_pos-abrufposition.

    LOOP AT gs_abruf_pos
      ASSIGNING FIELD-SYMBOL(<fs_pos>).

      IF <fs_pos>-abnahme_status NE '900'.
        EXIT.
      ELSE.
        gs_params-er_entity_kopf-status_abruf = '900'.
      ENDIF.

    ENDLOOP.

  ENDMETHOD.


  METHOD check_112_to_113.
*----------------------------------------------------------------------*
* SBB AG, SAP CCC-IT, Bern/Berne/Berna
*----------------------------------------------------------------------*
*
*-----------Änderungsdokumentation / documentation of changes----------*
* Version SAP-Rel Datum Bearbeiter
* Beschreibung Validate status Testplan Retrieved to Testplan Delivered
*----------------------------------------------------------------------*
* >000< 7.50 28.10.2020 Firma(DXC Technology) / Name(Shernan Bamba)
* Programmerstellung / creation of program
*----------------------------------------------------------------------*
* >001< 7.50 tt.mm.jjjj Firma(company) / Name(name)
* ...
*----------------------------------------------------------------------*

*Mapping:
    gs_params-er_entity_billing-abrufnummer = gs_params-er_entity_pos-abrufnummer.
    gs_params-er_entity_billing-abrufposition = gs_params-er_entity_pos-abrufposition.
    gs_params-er_entity_billing-lieferanteil = 'TSTPL'.
    gs_params-er_entity_billing-lieferdatum = gs_params-er_entity_pos-realisiertes_lieferdatum.
    gs_params-er_entity_billing-wert_position = gs_params-er_entity_pos-wert_position.
    gs_params-er_entity_billing-kosten_position = gs_params-er_entity_pos-kosten_position.
    gs_params-er_entity_billing-einheit_wert = gs_params-er_entity_pos-einheit_wert.
    gs_params-er_entity_billing-waehrung = gs_params-er_entity_pos-waehrung.

    SELECT SINGLE anteil
      FROM zrs_3tt_katlieft
      INTO @DATA(lv_anteil)
     WHERE lieferanteil EQ @gs_params-er_entity_billing-lieferanteil.

    IF sy-subrc NE 0.

      CLEAR lv_anteil.

    ELSE.

      gs_params-er_entity_billing-kosten_anteil = gs_params-er_entity_pos-kosten_position * ( lv_anteil / 100 ).
      gs_params-er_entity_billing-wert_pos_anteil = gs_params-er_entity_pos-wert_position * ( lv_anteil / 100 ).

    ENDIF.



*"Send email here: ZRS_TRICHTER_30007
*"Recipient: gs_params-er_entity_vorhaben-projektleiter
*"CC: gs_params-er_entity_vorhaben-Stv_projekleiter
*"Name/Value: ID = gs_params-er_entity_kopf-vorhabensnummer
*"            DESC = gs_params-er_entity_vorhaben-PROJEKTBEZEICHNUNG/o
*"            ABRUF gs_params-er_entity_pos-abrufnummer
    DATA(lo_vorhaben) = NEW zcl_rs_3vorhaben( ).

    gs_params-es_email_params-iv_mail_id = zif_rs_3constants=>gc_email_templates_abruf-et3112113. "ZRS_TRICHTER_30007

    gs_params-es_email_params-it_recipients = VALUE #(
      (
        address = lo_vorhaben->get_email_address( EXPORTING iv_userid =  gs_params-er_entity_vorhaben-service_spoc )
        address_type = zif_rs_3constants=>gc_email_kind-address_type_u
        recepient_type = zif_rs_3constants=>gc_email_kind-recepient_type_to
      )
      (
        address = lo_vorhaben->get_email_address( EXPORTING iv_userid =  gs_params-er_entity_vorhaben-stv_sspoc )
        address_type = zif_rs_3constants=>gc_email_kind-address_type_u
        recepient_type = zif_rs_3constants=>gc_email_kind-recepient_type_cc
      )
    ).

    DELETE gs_params-es_email_params-it_recipients
       WHERE address = space.

    gs_params-es_email_params-it_name_value = VALUE #(
      (
        name = zif_rs_3constants=>gc_email_namevalues_abruf-vorhabensnummer
        value = gs_params-er_entity_kopf-vorhabensnummer
      )
      (
        name = zif_rs_3constants=>gc_email_namevalues_abruf-projektbezeichnung
        value = gs_params-er_entity_vorhaben-projektbezeichnung
      )
      (
        name = zif_rs_3constants=>gc_email_namevalues_abruf-abrufnummer
        value = gs_params-er_entity_pos-abrufnummer
      )
    ).

  ENDMETHOD.


  METHOD check_113_to_114.
*----------------------------------------------------------------------*
* SBB AG, SAP CCC-IT, Bern/Berne/Berna
*----------------------------------------------------------------------*
*
*-----------Änderungsdokumentation / documentation of changes----------*
* Version SAP-Rel Datum Bearbeiter
* Beschreibung Validate status Testplan delivered to Testplan Accepted
*----------------------------------------------------------------------*
* >000< 7.50 28.10.2020 Firma(DXC Technology) / Name(Shernan Bamba)
* Programmerstellung / creation of program
*----------------------------------------------------------------------*
* >001< 7.50 tt.mm.jjjj Firma(company) / Name(name)
* ...
*----------------------------------------------------------------------*

    FREE: gs_params-es_email_params.
*The status 114 “Test plan approved” can be set for each position.
*I can select all / several positions and set the same status with a button.
*When saving, a warning is issued if not all positions get a status 114 or 112.
*As soon as the status 114 is set once, neither this nor all other fields in this position can be changed
*The acceptance date is updated for all positions with the status 114 “Test plan accepted” in the Allocation table. Field mapping see below.

    get_all_pos( ).

    LOOP AT gs_abruf_pos
      ASSIGNING FIELD-SYMBOL(<fs_pos>).

      IF <fs_pos>-abnahme_status NE '114'
      OR <fs_pos>-abnahme_status NE '112'.

        DATA(lo_message_container) = gs_params-es_mo_context->get_message_container( ).

        lo_message_container->add_message(
          EXPORTING
             iv_msg_id      = 'ZRS_3'
             iv_msg_number  = '000'
             iv_msg_type    = 'W'
             iv_add_to_response_header = abap_true
            ).

        EXIT.

      ENDIF.

    ENDLOOP.

*Mapping:
    gs_params-er_entity_billing-abrufnummer = gs_params-er_entity_pos-abrufnummer.
    gs_params-er_entity_billing-abrufposition = gs_params-er_entity_pos-abrufposition.
    gs_params-er_entity_billing-lieferanteil = 'TSTPL'.
    gs_params-er_entity_billing-abnahmedatum = sy-datum.

*"Send email here: ZRS_TRICHTER_30008
*"Recipient: gs_params-er_entity_vorhaben-projektleiter
*"CC: gs_params-er_entity_vorhaben-Stv_projekleiter
*"Name/Value: ID = gs_params-er_entity_kopf-vorhabensnummer
*"            DESC = gs_params-er_entity_vorhaben-PROJEKTBEZEICHNUNG/o
*"            ABRUF gs_params-er_entity_pos-abrufnummer
    DATA(lo_vorhaben) = NEW zcl_rs_3vorhaben( ).

    gs_params-es_email_params-iv_mail_id = zif_rs_3constants=>gc_email_templates_abruf-et3113114. "ZRS_TRICHTER_30008

    gs_params-es_email_params-it_recipients = VALUE #(
      (
        address = lo_vorhaben->get_email_address( EXPORTING iv_userid =  gs_params-er_entity_vorhaben-service_spoc )
        address_type = zif_rs_3constants=>gc_email_kind-address_type_u
        recepient_type = zif_rs_3constants=>gc_email_kind-recepient_type_to
      )
      (
        address = lo_vorhaben->get_email_address( EXPORTING iv_userid =  gs_params-er_entity_vorhaben-stv_sspoc )
        address_type = zif_rs_3constants=>gc_email_kind-address_type_u
        recepient_type = zif_rs_3constants=>gc_email_kind-recepient_type_cc
      )
    ).

    DELETE gs_params-es_email_params-it_recipients
       WHERE address = space.

    gs_params-es_email_params-it_name_value = VALUE #(
      (
        name = zif_rs_3constants=>gc_email_namevalues_abruf-vorhabensnummer
        value = gs_params-er_entity_kopf-vorhabensnummer
      )
      (
        name = zif_rs_3constants=>gc_email_namevalues_abruf-projektbezeichnung
        value = gs_params-er_entity_vorhaben-projektbezeichnung
      )
      (
        name = zif_rs_3constants=>gc_email_namevalues_abruf-abrufnummer
        value = gs_params-er_entity_pos-abrufnummer
      )
    ).

  ENDMETHOD.


  METHOD constructor.
*----------------------------------------------------------------------*
* SBB AG, SAP CCC-IT, Bern/Berne/Berna
*----------------------------------------------------------------------*
*
*-----------Änderungsdokumentation / documentation of changes----------*
* Version SAP-Rel Datum Bearbeiter
* Beschreibung Constructor
*----------------------------------------------------------------------*
* >000< 7.50 28.10.2020 Firma(DXC Technology) / Name(Shernan Bamba)
* Programmerstellung / creation of program
*----------------------------------------------------------------------*
* >001< 7.50 tt.mm.jjjj Firma(company) / Name(name)
* ...
*----------------------------------------------------------------------*
    gs_params = is_params.
  ENDMETHOD.


  METHOD CREATE.
    DATA(lo_status) = NEW zcl_rs_3abrufstatus_lp3( is_params ).
    rv_object = lo_status.
  ENDMETHOD.


  METHOD get_all_pos.
*----------------------------------------------------------------------*
* SBB AG, SAP CCC-IT, Bern/Berne/Berna
*----------------------------------------------------------------------*
*
*-----------Änderungsdokumentation / documentation of changes----------*
* Version SAP-Rel Datum Bearbeiter
* Beschreibung Get all positions based on abruf number
*----------------------------------------------------------------------*
* >000< 7.50 28.10.2020 Firma(DXC Technology) / Name(Shernan Bamba)
* Programmerstellung / creation of program
*----------------------------------------------------------------------*
* >001< 7.50 tt.mm.jjjj Firma(company) / Name(name)
* ...
*----------------------------------------------------------------------*

    SELECT *
      FROM zrs_3tt_abrufpos
      INTO TABLE gs_abruf_pos
     WHERE abrufnummer = gs_params-er_entity_kopf-abrufnummer.

    IF sy-subrc NE 0.

      FREE gs_abruf_pos.

    ENDIF.

  ENDMETHOD.


  METHOD get_billing.
*----------------------------------------------------------------------*
* SBB AG, SAP CCC-IT, Bern/Berne/Berna
*----------------------------------------------------------------------*
*
*-----------Änderungsdokumentation / documentation of changes----------*
* Version SAP-Rel Datum Bearbeiter
* Beschreibung Get billing values
*----------------------------------------------------------------------*
* >000< 7.50 28.10.2020 Firma(DXC Technology) / Name(Shernan Bamba)
* Programmerstellung / creation of program
*----------------------------------------------------------------------*
* >001< 7.50 tt.mm.jjjj Firma(company) / Name(name)
* ...
*----------------------------------------------------------------------*

*    SELECT SINGLE *
*      FROM zrs_3tt_verrchng
*      INTO gs_params-er_entity_billing
*     WHERE abrufnummer   EQ gs_params-er_entity_pos-abrufnummer
*       AND abrufposition EQ gs_params-er_entity_pos-abrufposition.
*
*    IF sy-subrc EQ 0.
*      CLEAR gs_params-er_entity_billing.
*    ENDIF.

  ENDMETHOD.


  METHOD get_kopf.
*----------------------------------------------------------------------*
* SBB AG, SAP CCC-IT, Bern/Berne/Berna
*----------------------------------------------------------------------*
*
*-----------Änderungsdokumentation / documentation of changes----------*
* Version SAP-Rel Datum Bearbeiter
* Beschreibung Get Header data
*----------------------------------------------------------------------*
* >000< 7.50 28.10.2020 Firma(DXC Technology) / Name(Shernan Bamba)
* Programmerstellung / creation of program
*----------------------------------------------------------------------*
* >001< 7.50 tt.mm.jjjj Firma(company) / Name(name)
* ...
*----------------------------------------------------------------------*

    SELECT SINGLE *
      FROM zrs_3tt_abrufkop
      INTO CORRESPONDING FIELDS OF gs_params-er_entity_kopf
     WHERE abrufnummer   EQ gs_params-er_entity_pos-abrufnummer.

    IF sy-subrc NE 0.
      CLEAR gs_params-er_entity_kopf.
    ENDIF.

  ENDMETHOD.


  METHOD get_project.
*----------------------------------------------------------------------*
* SBB AG, SAP CCC-IT, Bern/Berne/Berna
*----------------------------------------------------------------------*
*
*-----------Änderungsdokumentation / documentation of changes----------*
* Version SAP-Rel Datum Bearbeiter
* Beschreibung Get Project details
*----------------------------------------------------------------------*
* >000< 7.50 28.10.2020 Firma(DXC Technology) / Name(Shernan Bamba)
* Programmerstellung / creation of program
*----------------------------------------------------------------------*
* >001< 7.50 tt.mm.jjjj Firma(company) / Name(name)
* ...
*----------------------------------------------------------------------*

    DATA(lo_vorhaben) = NEW zcl_rs_3vorhaben( ).

    lo_vorhaben->get_project_details(
      EXPORTING
        iv_vorhabensnummer = gs_params-er_entity_kopf-vorhabensnummer
      IMPORTING
        es_vorhaben  = gs_params-er_entity_vorhaben
      ).

  ENDMETHOD.


  METHOD raise_error.
*----------------------------------------------------------------------*
* SBB AG, SAP CCC-IT, Bern/Berne/Berna
*----------------------------------------------------------------------*
*
*-----------Änderungsdokumentation / documentation of changes----------*
* Version SAP-Rel Datum Bearbeiter
* Beschreibung Raise error message
*----------------------------------------------------------------------*
* >000< 7.50 28.10.2020 Firma(DXC Technology) / Name(Shernan Bamba)
* Programmerstellung / creation of program
*----------------------------------------------------------------------*
* >001< 7.50 tt.mm.jjjj Firma(company) / Name(name)
* ...
*----------------------------------------------------------------------*

    DATA(lv_methodname) = is_methodname.
    DATA(lv_message) = CONV bapi_msg( is_message ).

    CASE is_opt.
      WHEN mc_error_opt-business.
        IF lv_message IS NOT INITIAL.
          RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
            EXPORTING
              textid  = is_textid
              message = lv_message.
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


  METHOD set_default_email_config.
*----------------------------------------------------------------------*
* SBB AG, SAP CCC-IT, Bern/Berne/Berna
*----------------------------------------------------------------------*
*
*-----------Änderungsdokumentation / documentation of changes----------*
* Version SAP-Rel Datum Bearbeiter
* Beschreibung Set Default Email config
*----------------------------------------------------------------------*
* >000< 7.50 28.10.2020 Firma(DXC Technology) / Name(Shernan Bamba)
* Programmerstellung / creation of program
*----------------------------------------------------------------------*
* >001< 7.50 tt.mm.jjjj Firma(company) / Name(name)
* ...
*----------------------------------------------------------------------*

    DATA(lo_vorhaben) = NEW zcl_rs_3vorhaben( ).
    gs_params-es_email_params-iv_sender_address = lo_vorhaben->get_email_address( EXPORTING iv_userid =  sy-uname ).
    gs_params-es_email_params-iv_sender_address_type = zif_rs_3constants=>gc_email_kind-address_type_u.
    gs_params-es_email_params-iv_language            = sy-langu.
    gs_params-es_email_params-iv_send_immediately    = 'X'.
    gs_params-es_email_params-iv_html                = 'X'.
    gs_params-es_email_params-iv_subject_as_title    = ''.

  ENDMETHOD.


METHOD validate.
*----------------------------------------------------------------------*
* SBB AG, SAP CCC-IT, Bern/Berne/Berna
*----------------------------------------------------------------------*
*
*-----------Änderungsdokumentation / documentation of changes----------*
* Version SAP-Rel Datum Bearbeiter
* Beschreibung LP3 Status Validation
*----------------------------------------------------------------------*
* >000< 7.50 28.10.2020 Firma(DXC Technology) / Name(Shernan Bamba)
* Programmerstellung / creation of program
*----------------------------------------------------------------------*
* >001< 7.50 tt.mm.jjjj Firma(company) / Name(name)
* ...
*----------------------------------------------------------------------*

  IF  gs_params-er_entity_kopf-status_abruf      EQ '15'
  AND gs_params-er_entity_pos_old-abnahme_status EQ '10'
  AND gs_params-er_entity_pos-abnahme_status     EQ '110'.

    check_10_to_110( ). "Trichter-57

  ELSEIF gs_params-er_entity_pos_old-abnahme_status EQ '110'
  AND    gs_params-er_entity_pos-abnahme_status     EQ '10'.

    check_110_to_10(  ). "Trichter-58

  ELSEIF gs_params-er_entity_pos_old-abnahme_status EQ '110'
  AND    gs_params-er_entity_pos-abnahme_status     EQ '111'.

    check_110_to_111(  ). "Trichter-59

  ELSEIF gs_params-er_entity_pos_old-abnahme_status EQ '111'
  AND    gs_params-er_entity_pos-abnahme_status     EQ '900'.

    check_111_to_900( ). "Trichter-60

  ELSEIF gs_params-er_entity_pos_old-abnahme_status EQ '111'
  AND    gs_params-er_entity_pos-abnahme_status     EQ '112'.

    check_111_to_112( ). "Trichter-61

  ELSEIF gs_params-er_entity_pos_old-abnahme_status EQ '112'
  AND    gs_params-er_entity_pos-abnahme_status     EQ '113'.

    check_112_to_113( ). "Trichter-62

  ELSEIF gs_params-er_entity_pos_old-abnahme_status EQ '113'
  AND    gs_params-er_entity_pos-abnahme_status     EQ '114'.

    check_113_to_114( ). "Trichter-64

  ENDIF.

ENDMETHOD.


  METHOD zif_rs_3dpc_abrufstatus~check.
*----------------------------------------------------------------------*
* SBB AG, SAP CCC-IT, Bern/Berne/Berna
*----------------------------------------------------------------------*
*
*-----------Änderungsdokumentation / documentation of changes----------*
* Version SAP-Rel Datum Bearbeiter
* Beschreibung LP3 Status Check
*----------------------------------------------------------------------*
* >000< 7.50 28.10.2020 Firma(DXC Technology) / Name(Shernan Bamba)
* Programmerstellung / creation of program
*----------------------------------------------------------------------*
* >001< 7.50 tt.mm.jjjj Firma(company) / Name(name)
* ...
*----------------------------------------------------------------------*
    DATA(lv_uname) = sy-uname.

*"Set email config
    set_default_email_config( ).

*"Get Order Header
    get_kopf( ).

*"Get Project
    get_project( ).
*"Get Billing
*    get_billing( ).

*"Validations
    validate( ).

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

  ENDMETHOD.


  METHOD zif_rs_3dpc_abrufstatus~get_results.
*----------------------------------------------------------------------*
* SBB AG, SAP CCC-IT, Bern/Berne/Berna
*----------------------------------------------------------------------*
*
*-----------Änderungsdokumentation / documentation of changes----------*
* Version SAP-Rel Datum Bearbeiter
* Beschreibung Get results
*----------------------------------------------------------------------*
* >000< 7.50 28.10.2020 Firma(DXC Technology) / Name(Shernan Bamba)
* Programmerstellung / creation of program
*----------------------------------------------------------------------*
* >001< 7.50 tt.mm.jjjj Firma(company) / Name(name)
* ...
*----------------------------------------------------------------------*
    es_entity_vorhaben = gs_params-er_entity_vorhaben.
    es_entity_kopf = gs_params-er_entity_kopf.
    es_entity_pos = gs_params-er_entity_pos.
    es_entity_billing = gs_params-er_entity_billing.
    es_entity_kopf_old = gs_params-er_entity_kopf_old.
    es_entity_pos_old = gs_params-er_entity_pos_old.
    es_email_params = gs_params-es_email_params.
    es_mo_context = gs_params-es_mo_context.
  ENDMETHOD.
ENDCLASS.
