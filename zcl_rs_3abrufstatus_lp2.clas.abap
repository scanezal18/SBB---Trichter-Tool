CLASS zcl_rs_3abrufstatus_lp2 DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_rs_3dpc_abrufstatus .

    CLASS-METHODS create
      IMPORTING
        !is_params       TYPE zif_rs_3dpc_abrufstatus=>ts_input_parameters
      RETURNING
        VALUE(rv_object) TYPE REF TO zif_rs_3dpc_abrufstatus
      RAISING
        /iwbep/cx_mgw_tech_exception
        /iwbep/cx_mgw_busi_exception .
    METHODS constructor
      IMPORTING
        !is_params TYPE zif_rs_3dpc_abrufstatus=>ts_input_parameters
      RAISING
        /iwbep/cx_mgw_tech_exception
        /iwbep/cx_mgw_busi_exception .
  PROTECTED SECTION.
  PRIVATE SECTION.

    TYPES:
      BEGIN OF ts_current_data,
        t_vorhaben  TYPE zif_rs_3dpc_abrufstatus=>tt_vorhaben,
        t_abrufkopf TYPE zif_rs_3dpc_abrufstatus=>tt_abrufkopf,
        t_abrufpos  TYPE zif_rs_3dpc_abrufstatus=>tt_abrufpos,
        t_verrchng  TYPE zif_rs_3dpc_abrufstatus=>tt_verrchng,
      END OF ts_current_data .
    TYPES:
      tt_bapibname TYPE STANDARD TABLE OF bapibname WITH DEFAULT KEY .

    DATA gs_params TYPE zif_rs_3dpc_abrufstatus=>ts_input_parameters .
    DATA gs_current_data TYPE ts_current_data .
    CONSTANTS:
      BEGIN OF mc_filter_values,
        status_filter          TYPE string VALUE '( Status eq `&` )' ##NO_TEXT,
        prjabrstat_filter      TYPE string VALUE '( Prozessart eq `@` and Status eq `&` )' ##NO_TEXT,
        mandt_filter           TYPE string VALUE '( Mandt eq `&` )' ##NO_TEXT,
        sprache_filter         TYPE string VALUE '( Sprache eq `&` )' ##NO_TEXT,
        abrufnummer_filter     TYPE string VALUE '( Abrufnummer eq `&` )' ##NO_TEXT,
        abrufpos_filter        TYPE string VALUE '( Abrufnummer eq `&` and Abrufposition eq `@` )' ##NO_TEXT,
        vorhabensnummer_filter TYPE string VALUE '( Vorhabensnummer eq `&` )' ##NO_TEXT,
        and                    TYPE string VALUE 'and' ##NO_TEXT,
        or                     TYPE string VALUE 'or' ##NO_TEXT,
        at_sign                TYPE string VALUE '@' ##NO_TEXT,
        ampersand              TYPE string VALUE '&' ##NO_TEXT,
        asterisk               TYPE string VALUE '*' ##NO_TEXT,
        is_edit_value          TYPE string VALUE 'IS_EDIT',
        is_abrufnummer_value   TYPE string VALUE 'ABRUFNUMMER',
        is_edit_trueformat     TYPE string VALUE `( IS_EDIT = 'X' )`,
        to_read_vorhaben       TYPE string VALUE '( Vorhabensnummer eq `&` and Status eq `@` )' ##NO_TEXT,
        to_read                TYPE string VALUE '( Abrufnummer eq `&` and Status eq `@` )' ##NO_TEXT,
      END OF mc_filter_values .
    CONSTANTS:
      BEGIN OF mc_error_opt,
        business  TYPE char1 VALUE 'B',
        technical TYPE char1 VALUE 'T',
      END OF mc_error_opt .

    METHODS get_data
      RAISING
        /iwbep/cx_mgw_tech_exception
        /iwbep/cx_mgw_busi_exception .
    METHODS set_data
      RAISING
        /iwbep/cx_mgw_tech_exception
        /iwbep/cx_mgw_busi_exception .
    METHODS validate_status_010
      RAISING
        /iwbep/cx_mgw_tech_exception
        /iwbep/cx_mgw_busi_exception .
    METHODS validate_status_020
      RAISING
        /iwbep/cx_mgw_tech_exception
        /iwbep/cx_mgw_busi_exception .
    METHODS validate_status_021
      RAISING
        /iwbep/cx_mgw_tech_exception
        /iwbep/cx_mgw_busi_exception .
    METHODS validate_status_022
      RAISING
        /iwbep/cx_mgw_tech_exception
        /iwbep/cx_mgw_busi_exception .
    METHODS validate_status_030
      RAISING
        /iwbep/cx_mgw_tech_exception
        /iwbep/cx_mgw_busi_exception .
    METHODS validate_status_031
      RAISING
        /iwbep/cx_mgw_tech_exception
        /iwbep/cx_mgw_busi_exception .
    METHODS validate_status_032
      RAISING
        /iwbep/cx_mgw_tech_exception
        /iwbep/cx_mgw_busi_exception .
    METHODS validate_status_035
      RAISING
        /iwbep/cx_mgw_tech_exception
        /iwbep/cx_mgw_busi_exception .
    METHODS validate_status_036
      RAISING
        /iwbep/cx_mgw_tech_exception
        /iwbep/cx_mgw_busi_exception .
    METHODS validate_status_037
      RAISING
        /iwbep/cx_mgw_tech_exception
        /iwbep/cx_mgw_busi_exception .
    METHODS validate_status_040
      RAISING
        /iwbep/cx_mgw_tech_exception
        /iwbep/cx_mgw_busi_exception .
    METHODS validate_status_041
      RAISING
        /iwbep/cx_mgw_tech_exception
        /iwbep/cx_mgw_busi_exception .
    METHODS validate_status_042
      RAISING
        /iwbep/cx_mgw_tech_exception
        /iwbep/cx_mgw_busi_exception .
    METHODS validate_status_900
      RAISING
        /iwbep/cx_mgw_tech_exception
        /iwbep/cx_mgw_busi_exception .
    METHODS validate_status_910
      RAISING
        /iwbep/cx_mgw_tech_exception
        /iwbep/cx_mgw_busi_exception .
    METHODS raise_error
      IMPORTING
        !is_opt               TYPE char1 DEFAULT mc_error_opt-business
        !is_textid            LIKE if_t100_message=>t100key DEFAULT /iwbep/cx_mgw_busi_exception=>business_error
        !is_message           TYPE string OPTIONAL
        !is_methodname        TYPE string OPTIONAL
        !is_omessagecontainer TYPE REF TO /iwbep/if_message_container OPTIONAL
      RAISING
        /iwbep/cx_mgw_tech_exception
        /iwbep/cx_mgw_busi_exception .
    METHODS set_abrufkopf
      RETURNING
        VALUE(rv_es_abrufkopf) TYPE zif_rs_3dpc_abrufstatus=>ts_abrufkopf
      RAISING
        /iwbep/cx_mgw_tech_exception
        /iwbep/cx_mgw_busi_exception .
    METHODS is_abrufkopf_cancelled
      RETURNING
        VALUE(rv_boolean) TYPE boolean
      RAISING
        /iwbep/cx_mgw_tech_exception
        /iwbep/cx_mgw_busi_exception .
    METHODS set_email_details
      IMPORTING
        !is_email_template_id  TYPE zif_rs_3dpc_abrufstatus=>ts_email_params-iv_mail_id
        !is_sender_id          TYPE bapibname-bapibname DEFAULT sy-uname
        !is_sender_email       TYPE string DEFAULT space
        !is_t_to_id            TYPE tt_bapibname
        !is_t_cc_id            TYPE tt_bapibname
      RETURNING
        VALUE(rv_email_params) TYPE zif_rs_3dpc_abrufstatus=>ts_email_params
      RAISING
        /iwbep/cx_mgw_tech_exception
        /iwbep/cx_mgw_busi_exception .
ENDCLASS.



CLASS ZCL_RS_3ABRUFSTATUS_LP2 IMPLEMENTATION.


  METHOD constructor.
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
    gs_params = is_params.
  ENDMETHOD.


  METHOD create.
    DATA(lo_status) = NEW zcl_rs_3abrufstatus_lp2( is_params ).
    rv_object = lo_status.
  ENDMETHOD.


  METHOD get_data.
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
    DATA: ls_kopf_params     TYPE zcl_rs_3abrufkopf=>ts_input_parameters,
          ls_pos_params      TYPE zcl_rs_3abrufpos=>ts_input_parameters,
          ls_verrchng_params TYPE zcl_rs_3verrchng=>ts_input_parameters,
          ls_status_params   TYPE zcl_rs_3prjabrstat=>ts_input_parameters.
    DATA(lv_osql_where_clause) = mc_filter_values-abrufnummer_filter.
    DATA(lv_abrufnummer) = gs_params-er_entity_pos-abrufnummer.
    IF lv_abrufnummer IS INITIAL.
      RETURN.
    ENDIF.

    REPLACE ALL OCCURRENCES OF mc_filter_values-ampersand IN lv_osql_where_clause WITH lv_abrufnummer.

    "get abufkopf
    gs_current_data-t_abrufkopf = NEW zcl_rs_3abrufkopf( ls_kopf_params )->get_db_values( lv_osql_where_clause ).

    "get abrufpos
    gs_current_data-t_abrufpos = NEW zcl_rs_3abrufpos( ls_pos_params )->get_db_values( lv_osql_where_clause ).

    IF gs_params-er_entity_pos-abrufposition IS NOT INITIAL.
      CLEAR lv_osql_where_clause.
      lv_osql_where_clause = mc_filter_values-abrufpos_filter.
      REPLACE ALL OCCURRENCES: OF mc_filter_values-ampersand IN lv_osql_where_clause WITH lv_abrufnummer,
                               OF mc_filter_values-at_sign IN lv_osql_where_clause WITH gs_params-er_entity_pos-abrufposition.
      "get billing
      gs_current_data-t_verrchng = NEW zcl_rs_3verrchng( ls_verrchng_params )->get_db_values( lv_osql_where_clause ).
    ENDIF.

    IF gs_current_data-t_abrufkopf IS NOT INITIAL.
      DATA(ls_abrufkopf) = gs_current_data-t_abrufkopf[ 1 ].
      CLEAR lv_osql_where_clause.
      lv_osql_where_clause = mc_filter_values-to_read_vorhaben.
      REPLACE ALL OCCURRENCES: OF mc_filter_values-ampersand IN lv_osql_where_clause WITH ls_abrufkopf-vorhabensnummer,
                               OF mc_filter_values-at_sign IN lv_osql_where_clause WITH zif_rs_3constants=>gc_vorhaben_status_active.
      "get vorhaben
      NEW zcl_rs_3vorhaben( )->get_all_projects( EXPORTING iv_condition = lv_osql_where_clause
                                                  IMPORTING et_vorhaben = gs_current_data-t_vorhaben ).

      IF gs_current_data-t_vorhaben IS INITIAL.
        raise_error( is_message = 'Vorhaben daten nicht aktiv.' ) ##NO_TEXT. "Vorhaben data is not active
      ENDIF.

      gs_params-er_entity_vorhaben = gs_current_data-t_vorhaben[ 1 ].
    ENDIF.

    CLEAR lv_osql_where_clause.
    lv_osql_where_clause = mc_filter_values-status_filter.
    REPLACE ALL OCCURRENCES OF mc_filter_values-ampersand IN lv_osql_where_clause WITH gs_params-er_entity_pos-abnahme_status.
    "get status table
    DATA(lt_status) = NEW zcl_rs_3prjabrstat( ls_status_params )->get_db_values( lv_osql_where_clause ).

    IF lt_status IS INITIAL.
      raise_error( is_message = 'Status ist nicht vorhanden.' ) ##NO_TEXT. "Status does not exists
    ENDIF.

    IF line_exists( gs_current_data-t_abrufpos[ abrufnummer = gs_params-er_entity_pos-abrufnummer
                                                abrufposition = gs_params-er_entity_pos-abrufposition ] ).
      gs_params-er_entity_pos_old = gs_current_data-t_abrufpos[ abrufnummer = gs_params-er_entity_pos-abrufnummer
                                                                abrufposition = gs_params-er_entity_pos-abrufposition ].
    ENDIF.
  ENDMETHOD.


  METHOD is_abrufkopf_cancelled.
    rv_boolean = abap_false.

    DATA(lt_abrufpos_cc) = gs_current_data-t_abrufpos.
    DELETE lt_abrufpos_cc WHERE abrufnummer = gs_params-er_entity_pos-abrufnummer
                             AND abrufposition = gs_params-er_entity_pos-abrufposition.
    DELETE lt_abrufpos_cc WHERE geloescht = abap_true.
    IF lt_abrufpos_cc IS NOT INITIAL.
      DELETE lt_abrufpos_cc WHERE abnahme_status = zif_rs_3constants=>gc_abrufpos_status_lp2-s900.
      IF lt_abrufpos_cc IS INITIAL.
        rv_boolean = abap_true.
      ENDIF.
    ELSE.
      rv_boolean = abap_true.
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


  METHOD set_abrufkopf.
    CLEAR rv_es_abrufkopf.

    IF gs_current_data-t_abrufkopf IS NOT INITIAL.
      IF line_exists( gs_current_data-t_abrufkopf[ abrufnummer = gs_params-er_entity_pos-abrufnummer ] ).
        DATA(ls_abrufkopf) = gs_current_data-t_abrufkopf[ abrufnummer = gs_params-er_entity_pos-abrufnummer ].
        CASE ls_abrufkopf-status.
          WHEN zif_rs_3constants=>gc_abrufkopf_status-s10.
            ls_abrufkopf-status = zif_rs_3constants=>gc_abrufkopf_status-s15.
            gs_params-er_entity_kopf = ls_abrufkopf.
          WHEN zif_rs_3constants=>gc_abrufkopf_status-s900 OR
               zif_rs_3constants=>gc_abrufkopf_status-s915.
            raise_error( is_message = 'AbrufKopf: Status darf nicht geändert werden' ) ##NO_TEXT. "Not allowed to change status
          WHEN OTHERS.
            " if same or others just continue and do not update
            CLEAR gs_params-er_entity_kopf.
        ENDCASE.
      ENDIF.
    ENDIF.

    IF ls_abrufkopf IS NOT INITIAL.
      rv_es_abrufkopf = ls_abrufkopf.
    ELSE.
      raise_error( is_message = 'AbrufKopf: Daten nicht gefunden.' ) ##NO_TEXT. "Data not found
    ENDIF.
  ENDMETHOD.


  METHOD set_data.
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
*    check if allowed to edit/locking validation
*    IF gs_params-is_cud_type = zif_rs_3constants=>gc_cud_types-update.
*      IF gs_params-er_entity_pos_old-lastchangedon <> gs_params-er_entity_pos-lastchangedon.
*        raise_error( is_message = 'Daten werden derzeit geändert. Bitte versuchen Sie es später noch einmal.' ). "Data is currently being modified. Please try again later.
*      ENDIF.
*    ENDIF.

    validate_status_010( ).
    validate_status_020( ).
    validate_status_021( ).
    validate_status_022( ).
    validate_status_030( ).
    validate_status_031( ).
    validate_status_032( ).
    validate_status_035( ).
    validate_status_036( ).
    validate_status_037( ).
    validate_status_040( ).
    validate_status_041( ).
    validate_status_042( ).
    validate_status_900( ).
    validate_status_910( ).
  ENDMETHOD.


  METHOD set_email_details.
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
    CLEAR rv_email_params.
    DATA(ls_email_params) = rv_email_params.
    DATA(lv_mail_id) = is_email_template_id.
    DATA(lv_sender_id) = is_sender_id.
    DATA(lt_to_id) = is_t_to_id.
    DATA(lt_cc_id) = is_t_cc_id.
    DATA(lv_langu) = sy-langu.
    DATA(lv_sender_email) = is_sender_email.
    CONDENSE lv_sender_email.
    IF lv_sender_email IS INITIAL.
      lv_sender_email = NEW zcl_rs_3vorhaben( )->get_email_address( iv_userid =  lv_sender_id ).
    ENDIF.

    ls_email_params-iv_mail_id = lv_mail_id.

    ls_email_params-iv_sender_address = lv_sender_email.
    ls_email_params-iv_sender_address_type = zif_rs_3constants=>gc_email_types-u.
    ls_email_params-iv_language            = lv_langu.
    ls_email_params-iv_send_immediately    = abap_true.
    ls_email_params-iv_html                = abap_true.
    CLEAR ls_email_params-iv_subject_as_title.

    TYPES: t_email_add TYPE STANDARD TABLE OF string WITH DEFAULT KEY.
    IF lt_to_id IS NOT INITIAL.

      APPEND LINES OF VALUE zif_rs_3dpc_abrufstatus=>ts_email_params-it_recipients( FOR ls_to_id IN lt_to_id
                             (
                               address = NEW zcl_rs_3vorhaben( )->get_email_address( EXPORTING iv_userid =  ls_to_id-bapibname )
                               address_type = zif_rs_3constants=>gc_email_kind-address_type_u
                               recepient_type = zif_rs_3constants=>gc_email_kind-recepient_type_to
                             )
                           ) TO ls_email_params-it_recipients.
    ENDIF.

    IF lt_cc_id IS NOT INITIAL.

      APPEND LINES OF VALUE zif_rs_3dpc_abrufstatus=>ts_email_params-it_recipients( FOR ls_cc_id IN lt_cc_id
                            (
                              address = NEW zcl_rs_3vorhaben( )->get_email_address( EXPORTING iv_userid =  ls_cc_id-bapibname )
                              address_type = zif_rs_3constants=>gc_email_kind-address_type_u
                              recepient_type = zif_rs_3constants=>gc_email_kind-recepient_type_cc
                            )
                          ) TO ls_email_params-it_recipients.
    ENDIF.

    IF ls_email_params-it_recipients IS NOT INITIAL.
      DELETE ls_email_params-it_recipients WHERE address = space.
    ENDIF.

    rv_email_params = ls_email_params.
  ENDMETHOD.


  METHOD validate_status_010.
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
    IF gs_params-er_entity_pos-abnahme_status <> zif_rs_3constants=>gc_abrufpos_status_lp2-s10.
      RETURN.
    ENDIF.
    DATA(lv_uname) = sy-uname.
    DATA(lv_datetoday) = sy-datum.

    CASE gs_params-is_cud_type.
      WHEN zif_rs_3constants=>gc_cud_types-create.
        IF gs_params-er_entity_pos-abrufnummer IS INITIAL.
          raise_error( is_message = 'Abruf Number darf nicht leer sein.' ) ##NO_TEXT. "Abruf Number must not be empty
        ENDIF.
        IF gs_params-er_entity_pos-abrufposition IS INITIAL.
          raise_error( is_message = 'Abruf Position darf nicht leer sein.' ) ##NO_TEXT. "Abruf Number must not be empty
        ENDIF.
        IF line_exists( gs_current_data-t_abrufpos[ abrufnummer = gs_params-er_entity_pos-abrufnummer
                                                    abrufposition = gs_params-er_entity_pos-abrufposition ] ).
          raise_error( is_message = 'Abruf doppelter Eintrag.' ) ##NO_TEXT. "Abruf duplicate entry
        ENDIF.
        gs_params-er_entity_pos-abrufer   = lv_uname.
        gs_params-er_entity_pos-abrufdatum = lv_datetoday.
      WHEN zif_rs_3constants=>gc_cud_types-update.
        IF gs_params-er_entity_pos-abrufnummer IS INITIAL.
          raise_error( is_message = 'Project Number darf nicht leer sein.' ) ##NO_TEXT. "Project Number must not be empty
        ENDIF.
        IF gs_params-er_entity_pos-abrufposition IS INITIAL.
          raise_error( is_message = 'Abruf Position darf nicht leer sein.' ) ##NO_TEXT. "Abruf Number must not be empty
        ENDIF.
        IF NOT line_exists( gs_current_data-t_abrufpos[ abrufnummer = gs_params-er_entity_pos-abrufnummer
                                                        abrufposition = gs_params-er_entity_pos-abrufposition ] ).
          raise_error( is_message = 'Abruf ist nicht vorhanden.' ) ##NO_TEXT. "Abruf does not exists
        ENDIF.
        IF gs_params-er_entity_pos_old IS INITIAL.
          raise_error( is_message = 'Abrufpos ist nicht vorhanden.' ) ##NO_TEXT. "Abruf does not exists
        ENDIF.
        gs_params-er_entity_pos-abrufer   = lv_uname.
        gs_params-er_entity_pos-abrufdatum = lv_datetoday.
      WHEN OTHERS.
        CLEAR gs_params-er_entity_pos.
    ENDCASE.
  ENDMETHOD.


  METHOD validate_status_020.
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
    IF gs_params-er_entity_pos-abnahme_status <> zif_rs_3constants=>gc_abrufpos_status_lp2-s20.
      RETURN.
    ENDIF.

    CASE gs_params-is_cud_type.
      WHEN zif_rs_3constants=>gc_cud_types-create.
        raise_error( is_message = 'Für diesen Status dürfen keine neuen Daten erstellt werden' ) ##NO_TEXT. "Not allowed to create a new data for this status
      WHEN zif_rs_3constants=>gc_cud_types-update.
        IF gs_params-er_entity_pos-abrufposition IS INITIAL.
          raise_error( is_message = 'Abruf Position darf nicht leer oder null sein' ) ##NO_TEXT. "Abruf Position must not be empty or zero
        ENDIF.
        IF gs_params-er_entity_pos-wert_objekt IS INITIAL.
          raise_error( is_message = 'Value Object darf nicht leer oder null sein' ) ##NO_TEXT. "Abruf Position must not be empty or zero
        ENDIF.
        IF gs_params-er_entity_pos-wert_position IS INITIAL.
          raise_error( is_message = 'Value Position darf nicht leer oder null sein' ) ##NO_TEXT. "Abruf Position must not be empty or zero
        ENDIF.
        IF gs_params-er_entity_pos-menge IS INITIAL.
          raise_error( is_message = 'Quantity darf nicht leer oder null sein' ) ##NO_TEXT. "Abruf Position must not be empty or zero
        ENDIF.
        IF gs_params-er_entity_pos-kosten_position IS INITIAL.
          raise_error( is_message = 'Cost Position darf nicht leer oder null sein' ) ##NO_TEXT. "Abruf Position must not be empty or zero
        ENDIF.
        IF gs_params-er_entity_pos-waehrung IS INITIAL.
          raise_error( is_message = 'Currency darf nicht leer oder null sein' ) ##NO_TEXT. "Abruf Position must not be empty or zero
        ENDIF.
        IF gs_params-er_entity_pos-geplantes_lieferdatum IS INITIAL.
          raise_error( is_message = 'Planned date of delivery darf nicht leer oder null sein' ) ##NO_TEXT. "Abruf Position must not be empty or zero
        ENDIF.
        IF gs_params-er_entity_pos_old IS INITIAL.
          raise_error( is_message = 'Abrufpos ist nicht vorhanden.' ) ##NO_TEXT. "Abrufpos does not exists
        ENDIF.

        DATA(ls_abrufkopf) = set_abrufkopf( ).

        CASE gs_params-er_entity_pos_old-abnahme_status.
          WHEN zif_rs_3constants=>gc_abrufpos_status_lp2-s10.

            gs_params-er_entity_pos-abrufdatum = sy-datum.
            gs_params-er_entity_pos-abrufer = sy-uname.

            "send email
            gs_params-es_email_params = set_email_details(
                   is_email_template_id = zif_rs_3constants=>gc_email_templates_abruf-et21020
                   is_t_to_id = VALUE tt_bapibname( ( bapibname = gs_params-er_entity_vorhaben-service_spoc ) )
                   is_t_cc_id = VALUE tt_bapibname( ( bapibname = gs_params-er_entity_vorhaben-stv_sspoc ) )
                   ).

            gs_params-es_email_params-it_name_value = VALUE #(
                            ( name = zif_rs_3constants=>gc_email_namevalues_abruf-vorhabensnummer
                              value = ls_abrufkopf-vorhabensnummer )
                            ( name = zif_rs_3constants=>gc_email_namevalues_abruf-projektbezeichnung
                              value =  gs_params-er_entity_vorhaben-projektbezeichnung )
                            ( name = zif_rs_3constants=>gc_email_namevalues_abruf-abrufnummer
                              value = ls_abrufkopf-abrufnummer  ) ).

          WHEN zif_rs_3constants=>gc_abrufpos_status_lp2-s21.
            CLEAR gs_params-er_entity_pos-realisiertes_lieferdatum.
            IF gs_current_data-t_verrchng IS NOT INITIAL.
              DATA(ls_billingdata) = gs_current_data-t_verrchng[ abrufnummer = gs_params-er_entity_pos-abrufnummer
                                                                 abrufposition = gs_params-er_entity_pos-abrufposition
                                                                 lieferanteil = zif_rs_3constants=>gc_katlieft_status-spez ].
              IF ls_billingdata-kz_verrechnet <> abap_true.
                CLEAR ls_billingdata-lieferdatum.
                ls_billingdata-kz_nichtabgenommen = abap_true.
                gs_params-er_entity_billing = ls_billingdata.
              ENDIF.
            ENDIF.

            "send email
            gs_params-es_email_params = set_email_details(
                   is_email_template_id = zif_rs_3constants=>gc_email_templates_abruf-et22120
                   is_t_to_id = VALUE tt_bapibname( ( bapibname = gs_params-er_entity_vorhaben-service_spoc ) )
                   is_t_cc_id = VALUE tt_bapibname( ( bapibname = gs_params-er_entity_vorhaben-stv_sspoc ) )
                   ).

            gs_params-es_email_params-it_name_value = VALUE #(
                          ( name = zif_rs_3constants=>gc_email_namevalues_abruf-vorhabensnummer
                            value = ls_abrufkopf-vorhabensnummer )
                          ( name = zif_rs_3constants=>gc_email_namevalues_abruf-projektbezeichnung
                            value =  gs_params-er_entity_vorhaben-projektbezeichnung )
                          ( name = zif_rs_3constants=>gc_email_namevalues_abruf-abrufnummer
                            value = ls_abrufkopf-abrufnummer  )
                          ( name = zif_rs_3constants=>gc_email_namevalues_abruf-notiz
                            value = gs_params-er_entity_pos-notiz  ) ).
          WHEN OTHERS.
            raise_error( is_message = 'Status ungultig.' ) ##NO_TEXT. "Status invalid
        ENDCASE.

      WHEN OTHERS.
        CLEAR gs_params-er_entity_pos.
    ENDCASE.
  ENDMETHOD.


  METHOD validate_status_021.
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
    IF gs_params-er_entity_pos-abnahme_status <> zif_rs_3constants=>gc_abrufpos_status_lp2-s21.
      RETURN.
    ENDIF.

    CASE gs_params-is_cud_type.
      WHEN zif_rs_3constants=>gc_cud_types-create.
        raise_error( is_message = 'Für diesen Status dürfen keine neuen Daten erstellt werden' ) ##NO_TEXT. "Not allowed to create a new data for this status
      WHEN zif_rs_3constants=>gc_cud_types-update.
        IF gs_params-er_entity_pos_old IS INITIAL.
          raise_error( is_message = 'Abrufpos ist nicht vorhanden.' ) ##NO_TEXT. "Abrufpos does not exists
        ENDIF.

        DATA(ls_abrufkopf) = set_abrufkopf( ).
        CASE gs_params-er_entity_pos_old-abnahme_status.
          WHEN zif_rs_3constants=>gc_abrufpos_status_lp2-s20.
            "send email
            gs_params-es_email_params = set_email_details(
                   is_email_template_id = zif_rs_3constants=>gc_email_templates_abruf-et22021
                   is_t_to_id = VALUE tt_bapibname( ( bapibname = gs_params-er_entity_vorhaben-projektleiter ) )
                   is_t_cc_id = VALUE tt_bapibname( ( bapibname = gs_params-er_entity_vorhaben-stv_projektleiter ) )
                   ).

            gs_params-es_email_params-it_name_value = VALUE #(
                          ( name = zif_rs_3constants=>gc_email_namevalues_abruf-vorhabensnummer
                            value = ls_abrufkopf-vorhabensnummer )
                          ( name = zif_rs_3constants=>gc_email_namevalues_abruf-projektbezeichnung
                            value =  gs_params-er_entity_vorhaben-projektbezeichnung )
                          ( name = zif_rs_3constants=>gc_email_namevalues_abruf-abrufnummer
                            value = ls_abrufkopf-abrufnummer  ) ).
          WHEN OTHERS.
            raise_error( is_message = 'Status ungultig.' ) ##NO_TEXT. "Status invalid
        ENDCASE.

        gs_params-er_entity_pos-realisiertes_lieferdatum = sy-datum.

      WHEN OTHERS.
        CLEAR gs_params-er_entity_pos.
    ENDCASE.
  ENDMETHOD.


  METHOD validate_status_022.
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
    IF gs_params-er_entity_pos-abnahme_status <> zif_rs_3constants=>gc_abrufpos_status_lp2-s22.
      RETURN.
    ENDIF.

    CASE gs_params-is_cud_type.
      WHEN zif_rs_3constants=>gc_cud_types-create.
        raise_error( is_message = 'Für diesen Status dürfen keine neuen Daten erstellt werden' ) ##NO_TEXT. "Not allowed to create a new data for this status
      WHEN zif_rs_3constants=>gc_cud_types-update.
        IF gs_params-er_entity_pos_old IS INITIAL.
          raise_error( is_message = 'Abrufpos ist nicht vorhanden.' ) ##NO_TEXT. "Abrufpos does not exists
        ENDIF.
        DATA(ls_abrufkopf) = set_abrufkopf( ).

        IF gs_params-er_entity_pos_old IS NOT INITIAL.
          CASE gs_params-er_entity_pos_old-abnahme_status.
            WHEN zif_rs_3constants=>gc_abrufpos_status_lp2-s21.

              gs_params-er_entity_billing = VALUE #( abrufnummer = gs_params-er_entity_pos-abrufnummer
                                                     abrufposition = gs_params-er_entity_pos-abrufposition
                                                     lieferanteil = zif_rs_3constants=>gc_katlieft_status-spez"'SPEZ'
                                                     lieferdatum = gs_params-er_entity_pos-realisiertes_lieferdatum
                                                     abnahmedatum = sy-datum
                                                     wert_position = gs_params-er_entity_pos-wert_position
                                                     kosten_position = gs_params-er_entity_pos-kosten_position
                                                     einheit_wert = gs_params-er_entity_pos-einheit_wert
                                                     waehrung = gs_params-er_entity_pos-waehrung ).

              DATA lv_params TYPE zcl_rs_3abrufpos=>ts_input_parameters.
              DATA(lv_anteil) = NEW zcl_rs_3abrufpos( lv_params )->get_katlief_anteil( gs_params-er_entity_billing-lieferanteil ).
              IF lv_anteil IS NOT INITIAL.
                gs_params-er_entity_billing-kosten_anteil = gs_params-er_entity_pos-kosten_position * lv_anteil.
                gs_params-er_entity_billing-wert_pos_anteil = gs_params-er_entity_pos-wert_position * lv_anteil.
              ENDIF.

              "send email
              gs_params-es_email_params = set_email_details(
                     is_email_template_id = zif_rs_3constants=>gc_email_templates_abruf-et22122
                     is_t_to_id = VALUE tt_bapibname( ( bapibname = gs_params-er_entity_vorhaben-service_spoc ) )
                     is_t_cc_id = VALUE tt_bapibname( ( bapibname = gs_params-er_entity_vorhaben-stv_sspoc ) )
                     ).

              gs_params-es_email_params-it_name_value = VALUE #(
                        ( name = zif_rs_3constants=>gc_email_namevalues_abruf-vorhabensnummer
                          value = ls_abrufkopf-vorhabensnummer )
                        ( name = zif_rs_3constants=>gc_email_namevalues_abruf-projektbezeichnung
                          value =  gs_params-er_entity_vorhaben-projektbezeichnung )
                        ( name = zif_rs_3constants=>gc_email_namevalues_abruf-abrufnummer
                          value = ls_abrufkopf-abrufnummer  ) ).
            WHEN OTHERS.
              raise_error( is_message = 'Status ungultig.' ) ##NO_TEXT. "Status invalid
          ENDCASE.
        ENDIF.
      WHEN OTHERS.
        CLEAR gs_params-er_entity_pos.
    ENDCASE.
  ENDMETHOD.


  METHOD validate_status_030.
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
    IF gs_params-er_entity_pos-abnahme_status <> zif_rs_3constants=>gc_abrufpos_status_lp2-s30.
      RETURN.
    ENDIF.

    CASE gs_params-is_cud_type.
      WHEN zif_rs_3constants=>gc_cud_types-create.
        raise_error( is_message = 'Für diesen Status dürfen keine neuen Daten erstellt werden' ) ##NO_TEXT. "Not allowed to create a new data for this status
      WHEN zif_rs_3constants=>gc_cud_types-update.
        IF gs_params-er_entity_pos_old IS INITIAL.
          raise_error( is_message = 'Abrufpos ist nicht vorhanden.' ) ##NO_TEXT. "Abrufpos does not exists
        ENDIF.

        DATA(ls_abrufkopf) = set_abrufkopf( ).
        CASE gs_params-er_entity_pos_old-abnahme_status.
          WHEN zif_rs_3constants=>gc_abrufpos_status_lp2-s22.
            " send email
          WHEN OTHERS.
            raise_error( is_message = 'Status ungultig.' ) ##NO_TEXT. "Status invalid
        ENDCASE.
      WHEN OTHERS.
        CLEAR gs_params-er_entity_pos.
    ENDCASE.
  ENDMETHOD.


  METHOD validate_status_031.
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
    IF gs_params-er_entity_pos-abnahme_status <> zif_rs_3constants=>gc_abrufpos_status_lp2-s31.
      RETURN.
    ENDIF.

    CASE gs_params-is_cud_type.
      WHEN zif_rs_3constants=>gc_cud_types-create.
        raise_error( is_message = 'Für diesen Status dürfen keine neuen Daten erstellt werden' ) ##NO_TEXT. "Not allowed to create a new data for this status
      WHEN zif_rs_3constants=>gc_cud_types-update.
        IF gs_params-er_entity_pos_old IS INITIAL.
          raise_error( is_message = 'Abrufpos ist nicht vorhanden.' ) ##NO_TEXT. "Abrufpos does not exists
        ENDIF.
        DATA(ls_abrufkopf) = set_abrufkopf( ).
      WHEN OTHERS.
        CLEAR gs_params-er_entity_pos.
    ENDCASE.
  ENDMETHOD.


  METHOD validate_status_032.
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
    IF gs_params-er_entity_pos-abnahme_status <> zif_rs_3constants=>gc_abrufpos_status_lp2-s32.
      RETURN.
    ENDIF.

    CASE gs_params-is_cud_type.
      WHEN zif_rs_3constants=>gc_cud_types-create.
        raise_error( is_message = 'Für diesen Status dürfen keine neuen Daten erstellt werden' ) ##NO_TEXT. "Not allowed to create a new data for this status
      WHEN zif_rs_3constants=>gc_cud_types-update.
        IF gs_params-er_entity_pos_old IS INITIAL.
          raise_error( is_message = 'Abrufpos ist nicht vorhanden.' ) ##NO_TEXT. "Abrufpos does not exists
        ENDIF.
        DATA(ls_abrufkopf) = set_abrufkopf( ).
      WHEN OTHERS.
        CLEAR gs_params-er_entity_pos.
    ENDCASE.
  ENDMETHOD.


  METHOD validate_status_035.
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
    IF gs_params-er_entity_pos-abnahme_status <> zif_rs_3constants=>gc_abrufpos_status_lp2-s35.
      RETURN.
    ENDIF.

    CASE gs_params-is_cud_type.
      WHEN zif_rs_3constants=>gc_cud_types-create.
        raise_error( is_message = 'Für diesen Status dürfen keine neuen Daten erstellt werden' ) ##NO_TEXT. "Not allowed to create a new data for this status
      WHEN zif_rs_3constants=>gc_cud_types-update.
        IF gs_params-er_entity_pos_old IS INITIAL.
          raise_error( is_message = 'Abrufpos ist nicht vorhanden.' ) ##NO_TEXT. "Abrufpos does not exists
        ENDIF.
        DATA(ls_abrufkopf) = set_abrufkopf( ).
      WHEN OTHERS.
        CLEAR gs_params-er_entity_pos.
    ENDCASE.
  ENDMETHOD.


  METHOD validate_status_036.
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
    IF gs_params-er_entity_pos-abnahme_status <> zif_rs_3constants=>gc_abrufpos_status_lp2-s36.
      RETURN.
    ENDIF.

    CASE gs_params-is_cud_type.
      WHEN zif_rs_3constants=>gc_cud_types-create.
        raise_error( is_message = 'Für diesen Status dürfen keine neuen Daten erstellt werden' ) ##NO_TEXT. "Not allowed to create a new data for this status
      WHEN zif_rs_3constants=>gc_cud_types-update.
        IF gs_params-er_entity_pos_old IS INITIAL.
          raise_error( is_message = 'Abrufpos ist nicht vorhanden.' ) ##NO_TEXT. "Abrufpos does not exists
        ENDIF.
        DATA(ls_abrufkopf) = set_abrufkopf( ).
      WHEN OTHERS.
        CLEAR gs_params-er_entity_pos.
    ENDCASE.
  ENDMETHOD.


  METHOD validate_status_037.
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
    IF gs_params-er_entity_pos-abnahme_status <> zif_rs_3constants=>gc_abrufpos_status_lp2-s37.
      RETURN.
    ENDIF.

    CASE gs_params-is_cud_type.
      WHEN zif_rs_3constants=>gc_cud_types-create.
        raise_error( is_message = 'Für diesen Status dürfen keine neuen Daten erstellt werden' ) ##NO_TEXT. "Not allowed to create a new data for this status
      WHEN zif_rs_3constants=>gc_cud_types-update.
        IF gs_params-er_entity_pos_old IS INITIAL.
          raise_error( is_message = 'Abrufpos ist nicht vorhanden.' ) ##NO_TEXT. "Abrufpos does not exists
        ENDIF.
        DATA(ls_abrufkopf) = set_abrufkopf( ).
      WHEN OTHERS.
        CLEAR gs_params-er_entity_pos.
    ENDCASE.
  ENDMETHOD.


  METHOD validate_status_040.
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
    IF gs_params-er_entity_pos-abnahme_status <> zif_rs_3constants=>gc_abrufpos_status_lp2-s40.
      RETURN.
    ENDIF.

    CASE gs_params-is_cud_type.
      WHEN zif_rs_3constants=>gc_cud_types-create.
        raise_error( is_message = 'Für diesen Status dürfen keine neuen Daten erstellt werden' ) ##NO_TEXT. "Not allowed to create a new data for this status
      WHEN zif_rs_3constants=>gc_cud_types-update.
        IF gs_params-er_entity_pos_old IS INITIAL.
          raise_error( is_message = 'Abrufpos ist nicht vorhanden.' ) ##NO_TEXT. "Abrufpos does not exists
        ENDIF.
        DATA(ls_abrufkopf) = set_abrufkopf( ).
      WHEN OTHERS.
        CLEAR gs_params-er_entity_pos.
    ENDCASE.
  ENDMETHOD.


  METHOD validate_status_041.
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
    IF gs_params-er_entity_pos-abnahme_status <> zif_rs_3constants=>gc_abrufpos_status_lp2-s41.
      RETURN.
    ENDIF.

    CASE gs_params-is_cud_type.
      WHEN zif_rs_3constants=>gc_cud_types-create.
        raise_error( is_message = 'Für diesen Status dürfen keine neuen Daten erstellt werden' ) ##NO_TEXT. "Not allowed to create a new data for this status
      WHEN zif_rs_3constants=>gc_cud_types-update.
        IF gs_params-er_entity_pos_old IS INITIAL.
          raise_error( is_message = 'Abrufpos ist nicht vorhanden.' ) ##NO_TEXT. "Abrufpos does not exists
        ENDIF.
        DATA(ls_abrufkopf) = set_abrufkopf( ).
      WHEN OTHERS.
        CLEAR gs_params-er_entity_pos.
    ENDCASE.
  ENDMETHOD.


  METHOD validate_status_042.
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
    IF gs_params-er_entity_pos-abnahme_status <> zif_rs_3constants=>gc_abrufpos_status_lp2-s42.
      RETURN.
    ENDIF.

    CASE gs_params-is_cud_type.
      WHEN zif_rs_3constants=>gc_cud_types-create.
        raise_error( is_message = 'Für diesen Status dürfen keine neuen Daten erstellt werden' ) ##NO_TEXT. "Not allowed to create a new data for this status
      WHEN zif_rs_3constants=>gc_cud_types-update.
        IF gs_params-er_entity_pos_old IS INITIAL.
          raise_error( is_message = 'Abrufpos ist nicht vorhanden.' ) ##NO_TEXT. "Abrufpos does not exists
        ENDIF.
        DATA(ls_abrufkopf) = set_abrufkopf( ).
      WHEN OTHERS.
        CLEAR gs_params-er_entity_pos.
    ENDCASE.
  ENDMETHOD.


  METHOD validate_status_900.
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
    IF gs_params-er_entity_pos-abnahme_status <> zif_rs_3constants=>gc_abrufpos_status_lp2-s900.
      RETURN.
    ENDIF.

    CASE gs_params-is_cud_type.
      WHEN zif_rs_3constants=>gc_cud_types-create.
        IF NOT line_exists( gs_current_data-t_abrufpos[ abrufnummer = gs_params-er_entity_pos-abrufnummer
                                                        abrufposition = gs_params-er_entity_pos-abrufposition ] ).
          raise_error( is_message = 'Abruf ist nicht vorhanden.' ) ##NO_TEXT. "Abruf does not exists
        ENDIF.
      WHEN zif_rs_3constants=>gc_cud_types-update.
        IF gs_params-er_entity_pos_old IS INITIAL.
          raise_error( is_message = 'Abrufpos ist nicht vorhanden.' ) ##NO_TEXT. "Abrufpos does not exists
        ENDIF.

        DATA(ls_abrufkopf) = set_abrufkopf( ).
        CASE gs_params-er_entity_pos_old-abnahme_status. " previous status
          WHEN zif_rs_3constants=>gc_abrufpos_status_lp2-s20.

            gs_params-er_entity_pos-rueckruf_datum = sy-datum.

            IF ls_abrufkopf IS NOT INITIAL AND is_abrufkopf_cancelled( ) = abap_true.
              gs_params-er_entity_kopf = ls_abrufkopf.
              gs_params-er_entity_kopf-status_abruf = zif_rs_3constants=>gc_abrufkopf_status-s900.
            ENDIF.


            gs_params-er_entity_billing = VALUE #( abrufnummer = gs_params-er_entity_pos-abrufnummer
                                                   abrufposition = gs_params-er_entity_pos-abrufposition
                                                   lieferanteil = zif_rs_3constants=>gc_katlieft_status-spez"'SPEZ'
                                                   lieferdatum = sy-datum
                                                   abnahmedatum = sy-datum
                                                   wert_position = gs_params-er_entity_pos-wert_position
                                                   kosten_position = gs_params-er_entity_pos-kosten_position
                                                   einheit_wert = gs_params-er_entity_pos-einheit_wert
                                                   waehrung = gs_params-er_entity_pos-waehrung ).

            DATA lv_params TYPE zcl_rs_3abrufpos=>ts_input_parameters.
            DATA(lv_anteil) = NEW zcl_rs_3abrufpos( lv_params )->get_katlief_anteil( gs_params-er_entity_billing-lieferanteil ).
            IF lv_anteil IS NOT INITIAL.
              gs_params-er_entity_billing-kosten_anteil = gs_params-er_entity_pos-kosten_position * lv_anteil.
              gs_params-er_entity_billing-wert_pos_anteil = gs_params-er_entity_pos-wert_position * lv_anteil.
            ENDIF.

            "send email
            gs_params-es_email_params = set_email_details(
                   is_email_template_id = zif_rs_3constants=>gc_email_templates_abruf-et220900
                   is_t_to_id = VALUE tt_bapibname( ( bapibname = gs_params-er_entity_vorhaben-service_spoc ) )
                   is_t_cc_id = VALUE tt_bapibname( ( bapibname = gs_params-er_entity_vorhaben-stv_sspoc ) )
                   ).

            gs_params-es_email_params-it_name_value = VALUE #(
                           ( name = zif_rs_3constants=>gc_email_namevalues_abruf-vorhabensnummer
                             value = ls_abrufkopf-vorhabensnummer )
                           ( name = zif_rs_3constants=>gc_email_namevalues_abruf-projektbezeichnung
                             value =  gs_params-er_entity_vorhaben-projektbezeichnung )
                           ( name = zif_rs_3constants=>gc_email_namevalues_abruf-abrufnummer
                             value = ls_abrufkopf-abrufnummer  ) ).

          WHEN zif_rs_3constants=>gc_abrufpos_status_lp2-s21.
            " send email
            gs_params-er_entity_pos-rueckruf_datum = sy-datum.
            IF ls_abrufkopf IS NOT INITIAL AND is_abrufkopf_cancelled( ) = abap_true.
              gs_params-er_entity_kopf = ls_abrufkopf.
              gs_params-er_entity_kopf-status_abruf = zif_rs_3constants=>gc_abrufkopf_status-s900.
            ENDIF.

            gs_params-er_entity_billing = VALUE #( abrufnummer = gs_params-er_entity_pos-abrufnummer
                                                   abrufposition = gs_params-er_entity_pos-abrufposition
                                                   lieferanteil = zif_rs_3constants=>gc_katlieft_status-spez"'SPEZ'
                                                   lieferdatum = sy-datum
                                                   abnahmedatum = sy-datum
                                                   wert_position = gs_params-er_entity_pos-wert_position
                                                   kosten_position = gs_params-er_entity_pos-kosten_position
                                                   einheit_wert = gs_params-er_entity_pos-einheit_wert
                                                   waehrung = gs_params-er_entity_pos-waehrung ).

            lv_anteil = NEW zcl_rs_3abrufpos( lv_params )->get_katlief_anteil( gs_params-er_entity_billing-lieferanteil ).
            IF lv_anteil IS NOT INITIAL.
              gs_params-er_entity_billing-kosten_anteil = gs_params-er_entity_pos-kosten_position * lv_anteil.
              gs_params-er_entity_billing-wert_pos_anteil = gs_params-er_entity_pos-wert_position * lv_anteil.
            ENDIF.

          WHEN zif_rs_3constants=>gc_abrufpos_status_lp2-s22.
            " send email
            gs_params-er_entity_pos-rueckruf_datum = sy-datum.
            IF ls_abrufkopf IS NOT INITIAL AND is_abrufkopf_cancelled( ) = abap_true.
              gs_params-er_entity_kopf = ls_abrufkopf.
              gs_params-er_entity_kopf-status_abruf = zif_rs_3constants=>gc_abrufkopf_status-s900.
            ENDIF.

          WHEN zif_rs_3constants=>gc_abrufpos_status_lp2-s32.
            " send email
            gs_params-er_entity_pos-rueckruf_datum = sy-datum.
            IF ls_abrufkopf IS NOT INITIAL AND is_abrufkopf_cancelled( ) = abap_true.
              gs_params-er_entity_kopf = ls_abrufkopf.
              gs_params-er_entity_kopf-status_abruf = zif_rs_3constants=>gc_abrufkopf_status-s900.
            ENDIF.

          WHEN zif_rs_3constants=>gc_abrufpos_status_lp2-s37.
            " send email
            gs_params-er_entity_pos-rueckruf_datum = sy-datum.

            IF ls_abrufkopf IS NOT INITIAL AND is_abrufkopf_cancelled( ) = abap_true.
              gs_params-er_entity_kopf = ls_abrufkopf.
              gs_params-er_entity_kopf-status_abruf = zif_rs_3constants=>gc_abrufkopf_status-s900.
            ENDIF.
          WHEN OTHERS.
        ENDCASE.
      WHEN OTHERS.
        CLEAR gs_params-er_entity_pos.
    ENDCASE.
  ENDMETHOD.


  METHOD validate_status_910.
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
    IF gs_params-er_entity_pos-abnahme_status <> zif_rs_3constants=>gc_abrufpos_status_lp2-s910.
      RETURN.
    ENDIF.

    CASE gs_params-is_cud_type.
      WHEN zif_rs_3constants=>gc_cud_types-create.
      WHEN zif_rs_3constants=>gc_cud_types-update.
      WHEN OTHERS.
        CLEAR gs_params-er_entity_pos.
    ENDCASE.
  ENDMETHOD.


  METHOD zif_rs_3dpc_abrufstatus~check.
    DATA(lv_uname) = sy-uname.
    CLEAR gs_params-es_email_params.
    IF gs_params-er_entity_pos IS INITIAL.
      RETURN.
    ENDIF.

    get_data( ).

    set_data( ).

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
