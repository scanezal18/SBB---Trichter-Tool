class ZCL_RS_3VORHABEN definition
  public
  final
  create public

  global friends ZCL_RS_3ABRUFSTATUS_LP3
                 ZIF_RS_3DPC_ABRUFSTATUS
                 ZIF_RS_3DPC_ENTITAT .

public section.

  interfaces ZIF_RS_3DPC_ANTWORT .
  interfaces ZIF_RS_3DPC_ENTITAT .

  types TS_VORHABEN type ZRS_3TT_VORHABEN_S .
  types:
    tt_vorhaben TYPE STANDARD TABLE OF ts_vorhaben WITH DEFAULT KEY .
  types:
    BEGIN OF ts_input_parameters,
        iv_entity_name                 TYPE string,
        iv_entity_set_name             TYPE string,
        iv_source_name                 TYPE string,
        it_key_tab                     TYPE /iwbep/t_mgw_name_value_pair,
        it_navigation_path             TYPE /iwbep/t_mgw_navigation_path,
        io_tech_request_context_create TYPE REF TO /iwbep/if_mgw_req_entity_c,
        io_tech_request_context_update TYPE REF TO /iwbep/if_mgw_req_entity_u,
        io_tech_request_context_delete TYPE REF TO /iwbep/if_mgw_req_entity_d,
        io_request_object_read         TYPE REF TO /iwbep/if_mgw_req_entity,
        io_tech_request_context_read   TYPE REF TO /iwbep/if_mgw_req_entity,
        io_tech_request_context_query  TYPE REF TO /iwbep/if_mgw_req_entityset,
        io_data_provider               TYPE REF TO /iwbep/if_mgw_entry_provider,
        es_response_context_read       TYPE /iwbep/if_mgw_appl_srv_runtime=>ty_s_mgw_response_entity_cntxt,
        es_response_context_set        TYPE /iwbep/if_mgw_appl_srv_runtime=>ty_s_mgw_response_context,
        it_filter_select_options       TYPE /iwbep/t_mgw_select_option,
        is_paging                      TYPE /iwbep/s_mgw_paging,
        it_order                       TYPE /iwbep/t_mgw_sorting_order,
        iv_filter_string               TYPE string,
        iv_search_string               TYPE string,
        er_entity                      TYPE ts_vorhaben,
        et_entityset                   TYPE tt_vorhaben,
        mo_context                     TYPE REF TO /iwbep/if_mgw_context,
        "expanded elements
        io_expand	                     TYPE REF TO /iwbep/if_mgw_odata_expand,
        er_entity_expand               TYPE REF TO data,
        er_entityset_expand            TYPE REF TO data,
        et_expanded_clauses	           TYPE string_table,
        et_expanded_tech_clauses       TYPE string_table,
        "deep entity
        er_deep_entity                 TYPE REF TO data,
      END OF ts_input_parameters .

  data GS_ROLE type ZRS_3TT_BENUTZERROLLEN_S .

  class-methods GET_USER_LIST
    importing
      !IT_FILTER type /IWBEP/T_MGW_SELECT_OPTION
    exporting
      !ET_USERS type ZRS_3TT_BAPIUSNAME_T .
  methods CREATE_PROJECT
    importing
      !IS_PROJECT type ZRS_3TT_VORHABEN
    exporting
      !ES_PROJECT type ZRS_3TT_VORHABEN_S
      !ET_RETURN type BAPIRET2_T
    raising
      /IWBEP/CX_MGW_TECH_EXCEPTION
      /IWBEP/CX_MGW_BUSI_EXCEPTION .
  methods UPDATE_PROJECT
    importing
      !IS_PROJECT type ZRS_3TT_VORHABEN
    exporting
      !ES_PROJECT type ZRS_3TT_VORHABEN_S
      !ET_RETURN type BAPIRET2_T
    raising
      /IWBEP/CX_MGW_TECH_EXCEPTION
      /IWBEP/CX_MGW_BUSI_EXCEPTION .
  methods GET_ALL_PROJECTS
    importing
      !IV_CONDITION type STRING
    exporting
      !ET_VORHABEN type ZRS_3TT_VORHABEN_TEXT_T .
  methods GET_PROJECT_DETAILS
    importing
      !IV_VORHABENSNUMMER type ZRS_3TT_VORHABEN-VORHABENSNUMMER
    exporting
      !ES_VORHABEN type ZRS_3TT_VORHABEN_S .
  class-methods VERIFY_USER
    importing
      !IV_USERNAME type BAPIBNAME-BAPIBNAME
    returning
      value(RV_RETURN) type CHAR1 .
  class-methods GET_ALL_PROJECTS_ASSIGNED
    exporting
      !ET_VORHABEN type ZRS_3TT_VORHABEN_TEXT_T .
  methods GET_DOMAIN_VALUES
    importing
      !IV_DOM_NAME type DD07L-DOMNAME
      !IV_LANGU type DD07T-DDLANGUAGE optional
      !IV_BOOL type DDREFSTRUC-BOOL optional
    exporting
      !ET_DOM_VALUES type ZRS_3TT_DOMAINWERT_T .
  methods GET_ALL_STATUS
    importing
      !IV_SPRACHE type SPRAS
      !IV_PROZESSART type ZRS_3TT_DE_PROZESSART
      !IV_USERID type SY-UNAME optional
      !IV_STATUS type ZRS_3TT_PRJABRST-ESTAT optional
    exporting
      !ET_STATUS_LIST type ZRS_3TT_PRJABRST_T .
  methods GET_ACCOUNTING_ELEMENT
    importing
      !IT_FILTER_SELECT_OPTIONS type /IWBEP/T_MGW_SELECT_OPTION optional
      !IV_ELEMENT_ID type TEXT40 optional
      !IV_TYPE type ZRS_3TT_XCOTYPE optional
    exporting
      !ET_ENTITYSET type ZRS_3TT_KONTELEM_DATA_T
      !ET_RETURN type BAPIRET2_T
    raising
      /IWBEP/CX_MGW_TECH_EXCEPTION
      /IWBEP/CX_MGW_BUSI_EXCEPTION .
  class-methods GET_STATUS
    importing
      !IV_SPRACHE type SPRAS
      !IV_PROZESSART type ZRS_3TT_DE_PROZESSART
      !IV_USERID type SY-UNAME optional
    exporting
      !ET_STATUS_LIST type ZRS_3TT_PRJABRST_T .
  methods CONSTRUCTOR
    importing
      !IS_PARAMS type TS_INPUT_PARAMETERS optional
      !IV_USERNAME type UNAME optional
    exceptions
      PROJECT_ID_NOT_FOUND .
  methods GET_ROLE
    importing
      !IT_KEYS type /IWBEP/T_MGW_TECH_PAIRS optional
      !IV_USERID type UNAME optional
    exporting
      !ET_ROLES type ZRS_3TT_UROLES_T
      !ET_RETURN type BAPIRET2_T
      value(ES_ENTITY) type ZRS_3TT_BENUTZERROLLEN_S .
  class-methods CREATE
    importing
      !IS_PARAMS type TS_INPUT_PARAMETERS
    returning
      value(RO_ENTITAT) type ref to ZIF_RS_3DPC_ENTITAT .
  methods GET_ACCT_ELEM_TYPE_TEXT
    importing
      !IV_ACCT_ELEM_TYPE type ZRS_3TT_DE_TYPVERCHEL
    exporting
      !EV_TEXT type ZRS_3TT_DE_TYPVERCHEL_T .
  methods GET_STATUS_TEXT
    importing
      !IV_SPRACHE type SPRAS
      !IV_PROZESSART type ZRS_3TT_DE_PROZESSART
      !IV_STATUS type ZRS_3TT_DE_STATUS
    exporting
      !EV_STATUSBESCHREIBUNG type ZRS_3TT_DE_STTBESCHRBNG .
protected section.
private section.

  data GS_PROJECT_DETAILS type ZRS_3TT_VORHABEN .
  data GS_SENDER_EMAIL type BAPIADDR3-E_MAIL .
  data GS_PARAMS type TS_INPUT_PARAMETERS .
  constants MC_KEY_FIELD type STRING value 'VORHABENSNUMMER' ##NO_TEXT.
  constants:
    BEGIN OF mc_error_opt,
        business  TYPE char1 VALUE 'B',
        technical TYPE char1 VALUE 'T',
      END OF mc_error_opt .

  methods GENERATE_PROJECT_ID
    exporting
      !EV_VORHABENSNUMMER type ZRS_3TT_DE_VRHBNSNMMR .
  class-methods GET_NAME
    importing
      !IV_USERNAME type BAPIBNAME-BAPIBNAME
    exporting
      !EV_FULLNAME type BAPIADDR3-FULLNAME
      !EV_FIRSTNAME type BAPIADDR3-FIRSTNAME
      !EV_LASTNAME type BAPIADDR3-LASTNAME .
  methods SEND_EMAIL
    importing
      !IV_MAIL_ID type ZSE_MAIL_ID
      !IV_PROJECT_ID type ZRS_3TT_VORHABEN-VORHABENSNUMMER optional
      !IV_SENDER_ADDRESS type STRING
      !IV_SENDER_ADDRESS_TYPE type SO_ESCAPE default 'B'
      !IV_LANGUAGE type SPRAS default SY-LANGU
      !IV_SEND_IMMEDIATELY type FLAG optional
      !IV_HTML type FLAG default 'X'
      !IV_SUBJECT_AS_TITLE type FLAG optional
      !IT_NAME_VALUE type ZRS_3TT_NAME_VALUE_T optional
      !IT_RECIPIENTS type ZRS_3TT_MAIL_REC_T optional
      !IT_TABLE type STANDARD TABLE optional
      !IT_TEXT_TABLE type SOLI_TAB optional
    exporting
      !EV_ERROR type FLAG
    changing
      !CT_MESSAGES type BAPIRET2_T .
  methods GET_EMAIL_ADDRESS
    importing
      !IV_USERID type BAPIBNAME-BAPIBNAME
    returning
      value(RV_EMAIL) type BAPIADDR3-E_MAIL .
  methods CHECK_FORECAST
    importing
      !IS_PROJECT type ZRS_3TT_VORHABEN_S optional
    changing
      !IT_PROJECT type ZRS_3TT_VORHABEN_TEXT_T optional
    returning
      value(RO_VALUE) type FLAG .
  methods GET_DATA
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



CLASS ZCL_RS_3VORHABEN IMPLEMENTATION.


  METHOD check_forecast.
*----------------------------------------------------------------------*
* SBB AG, SAP CCC-IT, Bern/Berne/Berna
*----------------------------------------------------------------------*
*
*-----------Änderungsdokumentation / documentation of changes----------*
* Version SAP-Rel Datum Bearbeiter
* Beschreibung Check if forecast exists
*----------------------------------------------------------------------*
* >000< 7.50 28.10.2020 Firma(DXC Technology) / Name(Shernan Bamba)
* Programmerstellung / creation of program
*----------------------------------------------------------------------*
* >001< 7.50 tt.mm.jjjj Firma(company) / Name(name)
* ...
*----------------------------------------------------------------------*

    IF is_project IS NOT INITIAL.

      SELECT *
        FROM zrs_3tt_projfcst
        INTO TABLE @DATA(lt_forecast)
       WHERE vorhabensnummer EQ @is_project-vorhabensnummer.

      IF sy-subrc EQ 0.

        LOOP AT lt_forecast
          ASSIGNING FIELD-SYMBOL(<fs_forecast>)
          WHERE forecast_aw NE 0
          OR    forecast_tp NE 0.

          ro_value = abap_true.

          EXIT.

        ENDLOOP.

      ENDIF.

    ELSEIF it_project[] IS NOT INITIAL.

      SELECT *
        FROM zrs_3tt_projfcst
        INTO TABLE lt_forecast
        FOR ALL ENTRIES IN it_project
       WHERE vorhabensnummer EQ it_project-vorhabensnummer.

      IF sy-subrc EQ 0.

        LOOP AT it_project
          ASSIGNING FIELD-SYMBOL(<fs_project>).

          LOOP AT lt_forecast
            ASSIGNING <fs_forecast>
            WHERE vorhabensnummer EQ <fs_project>-vorhabensnummer
            AND ( forecast_aw NE 0
            OR    forecast_tp NE 0 ).

            <fs_project>-forecast_maint = abap_true.

            EXIT.

          ENDLOOP.

        ENDLOOP.

      ENDIF.

    ENDIF.

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

    DATA:
      ls_address TYPE bapiaddr3,
      lt_return  TYPE STANDARD TABLE OF bapiret2,
      lt_roles   TYPE zrs_3tt_uroles_t.

    gs_params = is_params.
*"Get Role
    get_role(
      EXPORTING
        iv_userid = iv_username
      IMPORTING
        et_roles   = lt_roles
        et_return  = lt_return
        es_entity  = gs_role ).

*"Get Name
    CALL METHOD get_name
      EXPORTING
        iv_username  = iv_username
      IMPORTING
        ev_fullname  = gs_role-fullname
        ev_firstname = gs_role-firstname
        ev_lastname  = gs_role-lastname.

    gs_role-userid = iv_username.

*"Get Email
    CALL FUNCTION 'BAPI_USER_GET_DETAIL'
      EXPORTING
        username = sy-uname
      IMPORTING
        address  = ls_address
      TABLES
        return   = lt_return.

    IF sy-subrc EQ 0.

      gs_sender_email = ls_address-e_mail.

    ENDIF.

  ENDMETHOD.


  method CREATE.
*----------------------------------------------------------------------*
* SBB AG, SAP CCC-IT, Bern/Berne/Berna
*----------------------------------------------------------------------*
*
*-----------Änderungsdokumentation / documentation of changes----------*
* Version SAP-Rel Datum Bearbeiter
* Beschreibung Create instance
*----------------------------------------------------------------------*
* >000< 7.50 28.10.2020 Firma(DXC Technology) / Name(Shernan Bamba)
* Programmerstellung / creation of program
*----------------------------------------------------------------------*
* >001< 7.50 tt.mm.jjjj Firma(company) / Name(name)
* ...
*----------------------------------------------------------------------*

    DATA(lo_vorhaben) = NEW zcl_rs_3vorhaben( is_params = is_params ).

    ro_entitat = lo_vorhaben.

  endmethod.


  METHOD create_project.
*----------------------------------------------------------------------*
* SBB AG, SAP CCC-IT, Bern/Berne/Berna
*----------------------------------------------------------------------*
*
*-----------Änderungsdokumentation / documentation of changes----------*
* Version SAP-Rel Datum Bearbeiter
* Beschreibung Trichtertool: Utility class for project creation
*----------------------------------------------------------------------*
* >000< 7.50 28.10.2020 Firma(DXC Technology) / Name(Shernan Bamba)
* Programmerstellung / creation of program
*----------------------------------------------------------------------*
* >001< 7.50 tt.mm.jjjj Firma(company) / Name(name)
* ...
*----------------------------------------------------------------------*

    DATA: lt_zuvoranw   TYPE STANDARD TABLE OF zrs_3tt_zuvoranw,
          ls_zuvoranw   TYPE zrs_3tt_zuvoranw,
          lt_messages   TYPE bapiret2_t,
          lv_mail_error TYPE flag,
          lt_recipients TYPE STANDARD TABLE OF zse_2_mail_rec,
          lt_name_value TYPE STANDARD TABLE OF zse_name_value.

    DATA(ls_project) = is_project.

*"Generate Project ID
    me->generate_project_id(
      IMPORTING
        ev_vorhabensnummer = ls_project-vorhabensnummer ).

    IF ls_project-vorhabensnummer IS INITIAL.

      MESSAGE e003(zrs_3) INTO DATA(lv_message).
      raise_error( is_message = lv_message ).

      et_return = VALUE #( (
                    type = zif_rs_3constants=>gc_message_type_error
                    id = zif_rs_3constants=>gc_message_class
                    number = '003' "Project number generation error
                    message = lv_message
                    ) ).

    ENDIF.

*"Populate client number
    ls_project-mandt = sy-mandt.

*"Save to zrs_3tt_vorhaben
    INSERT zrs_3tt_vorhaben
      FROM ls_project.

    IF sy-subrc EQ 0.

*"Save to project assignment table
      lt_zuvoranw = VALUE #( ( mandt = sy-mandt
                               vorhabensnummer = ls_project-vorhabensnummer
                               userid = ls_project-projektleiter "Projektleiter
                               gueltig_ab = sy-datum
                               gueltig_bis = '99991231'
                               zrs_ttool = zif_rs_3constants=>gc_role_plswa )
                             (
                               mandt = sy-mandt
                               vorhabensnummer = ls_project-vorhabensnummer
                               userid = ls_project-stv_projektleiter "STV Projectleiter
                               gueltig_ab = sy-datum
                               gueltig_bis = '99991231'
                               zrs_ttool = zif_rs_3constants=>gc_role_plswa
                             )
                             (
                               mandt = sy-mandt
                               vorhabensnummer = ls_project-vorhabensnummer
                               userid = ls_project-software_architekt "Software Architekt
                               gueltig_ab = sy-datum
                               gueltig_bis = '99991231'
                               zrs_ttool = zif_rs_3constants=>gc_role_plswa
                             )
                             (
                               mandt = sy-mandt
                               vorhabensnummer = ls_project-vorhabensnummer
                               userid = ls_project-stv_swa  "STV SWA
                               gueltig_ab = sy-datum
                               gueltig_bis = '99991231'
                               zrs_ttool = zif_rs_3constants=>gc_role_plswa
                             )
                             (
                               mandt = sy-mandt
                               vorhabensnummer = ls_project-vorhabensnummer
                               userid = ls_project-service_spoc "Service Spoc
                               gueltig_ab = sy-datum
                               gueltig_bis = '99991231'
                               zrs_ttool = zif_rs_3constants=>gc_role_devspoc
                             )
                             (
                               mandt = sy-mandt
                               vorhabensnummer = ls_project-vorhabensnummer
                               userid = ls_project-stv_sspoc "Stv Sspoc
                               gueltig_ab = sy-datum
                               gueltig_bis = '99991231'
                               zrs_ttool = zif_rs_3constants=>gc_role_devspoc
                             ) ).

      SORT lt_zuvoranw.

      DELETE ADJACENT DUPLICATES FROM lt_zuvoranw
        COMPARING ALL FIELDS.

      DELETE lt_zuvoranw
        WHERE userid = space.

      INSERT zrs_3tt_zuvoranw
        FROM TABLE lt_zuvoranw.

      IF sy-subrc EQ 0.

        MOVE-CORRESPONDING ls_project TO es_project.

        et_return = VALUE #( (
                      type = zif_rs_3constants=>gc_message_type_success
                      id = zif_rs_3constants=>gc_message_class
                      number = '001' "Project number &1 created
                      message = ls_project-vorhabensnummer
                      ) ).

*"create forecast template into forecast table based on
        "project header dates
        CALL METHOD zcl_rs_3vorhabenforecast=>create_forecast
          EXPORTING
            is_project = ls_project.
*"Send Email
*        lt_recipients = VALUE #( (
*                          address = at_sender_email
*                          address_type = 'U'
*                          recepient_type = 'A'
*                          )
*                          (
*                          address = me->get_email_address( EXPORTING iv_userid = CONV #( ls_project-projektleiter ) )
*                          address_type = 'U'
*                          recepient_type = 'A'
*                          )
*                          (
*                          address = me->get_email_address( EXPORTING iv_userid = CONV #( ls_project-stv_projektleiter ) )
*                          address_type = 'U'
*                          recepient_type = 'A'
*                          )
*                          (
*                          address = me->get_email_address( EXPORTING iv_userid = CONV #( ls_project-software_architekt ) )
*                          address_type = 'U'
*                          recepient_type = 'A'
*                          )
*                          (
*                          address = me->get_email_address( EXPORTING iv_userid = CONV #( ls_project-stv_swa ) )
*                          address_type = 'U'
*                          recepient_type = 'A'
*                          )
*                          (
*                          address = me->get_email_address( EXPORTING iv_userid = CONV #( ls_project-service_spoc ) )
*                          address_type = 'U'
*                          recepient_type = 'A'
*                          )
*                          (
*                          address = me->get_email_address( EXPORTING iv_userid = CONV #( ls_project-stv_sspoc ) )
*                          address_type = 'U'
*                          recepient_type = 'A'
*                          )
*                          ).
*
*        DELETE lt_recipients
*          WHERE address = space.
*
*        lt_name_value = VALUE #( (
*                          name = 'ID'
*                          value = ls_project-vorhabensnummer
*                          ) ).
*
*        me->send_email(
*          EXPORTING
*            iv_mail_id             = zif_rs_3constants=>gc_email_project_create
*            iv_sender_address      = CONV #( sy-uname )
*            iv_sender_address_type = 'B' "SAP User
*            iv_language            = sy-langu
*            iv_send_immediately    = 'X'
*            iv_html                = 'X'
*            iv_subject_as_title    = 'X'
*            it_name_value          = lt_name_value
*            it_recipients          = lt_recipients
*           IMPORTING
*            ev_error               = lv_mail_error
*          CHANGING
*            ct_messages            = lt_messages
*            ).

      ELSE.

        MESSAGE e002(zrs_3) INTO lv_message.
        raise_error( is_message = lv_message ).

        et_return = VALUE #( (
                       type = zif_rs_3constants=>gc_message_type_error
                       id = zif_rs_3constants=>gc_message_class
                       number = '002' "Project creation error
                       message = ls_project-vorhabensnummer
                       ) ).

      ENDIF.

    ELSE.

      MESSAGE e002(zrs_3) INTO lv_message.
      raise_error( is_message = lv_message ).

      et_return = VALUE #( (
                    type = zif_rs_3constants=>gc_message_type_error
                    id = zif_rs_3constants=>gc_message_class
                    number = '002' "Project creation error
                    message = ls_project-vorhabensnummer
                    ) ).

    ENDIF.

  ENDMETHOD.


  METHOD generate_project_id.
*----------------------------------------------------------------------*
* SBB AG, SAP CCC-IT, Bern/Berne/Berna
*----------------------------------------------------------------------*
*
*-----------Änderungsdokumentation / documentation of changes----------*
* Version SAP-Rel Datum Bearbeiter
* Beschreibung Generate new Project Number class using ZRS_3TTPRJ NRO
*----------------------------------------------------------------------*
* >000< 7.50 28.10.2020 Firma(DXC Technology) / Name(Shernan Bamba)
* Programmerstellung / creation of program
*----------------------------------------------------------------------*
* >001< 7.50 tt.mm.jjjj Firma(company) / Name(name)
* ...
*----------------------------------------------------------------------*

    DATA: lv_number TYPE num10.

    CALL FUNCTION 'NUMBER_GET_NEXT'
      EXPORTING
        nr_range_nr             = zif_rs_3constants=>gc_vorhaben_nr_range
        object                  = zif_rs_3constants=>gc_vorhaben_nr_object
      IMPORTING
        number                  = lv_number
      EXCEPTIONS
        interval_not_found      = 1
        number_range_not_intern = 2
        object_not_found        = 3
        quantity_is_0           = 4
        quantity_is_not_1       = 5
        interval_overflow       = 6
        buffer_overflow         = 7
        OTHERS                  = 8.

    IF sy-subrc EQ 0.

      ev_vorhabensnummer = lv_number.

    ENDIF.

  ENDMETHOD.


  METHOD get_accounting_element.
*----------------------------------------------------------------------*
* SBB AG, SAP CCC-IT, Bern/Berne/Berna
*----------------------------------------------------------------------*
*
*-----------Änderungsdokumentation / documentation of changes----------*
* Version SAP-Rel Datum Bearbeiter
* Beschreibung Get accounting element and type
*----------------------------------------------------------------------*
* >000< 7.50 28.10.2020 Firma(DXC Technology) / Name(Shernan Bamba)
* Programmerstellung / creation of program
*----------------------------------------------------------------------*
* >001< 7.50 tt.mm.jjjj Firma(company) / Name(name)
* ...
*----------------------------------------------------------------------*

    DATA:
      lt_kont   TYPE zrs_3tt_kontelem_data_t,
      ls_return TYPE bapiret2,
      lv_search TYPE text40,
      lv_type   TYPE zrs_3tt_xcotype.

    IF it_filter_select_options[] IS NOT INITIAL.

      LOOP AT it_filter_select_options
        INTO DATA(ls_filter).

        CASE ls_filter-property.

          WHEN 'TYPE'.

            LOOP AT ls_filter-select_options
              INTO DATA(ls_select_options).

              IF ls_select_options-low EQ 'A'. "Alle

                CLEAR lv_type.

              ELSE.

                MOVE ls_select_options-low TO lv_type.

              ENDIF.

            ENDLOOP.

          WHEN 'KONT_ELEM_ID'.

            LOOP AT ls_filter-select_options
              INTO ls_select_options.

              MOVE ls_select_options-low TO lv_search.

            ENDLOOP.

          WHEN OTHERS.

        ENDCASE.

      ENDLOOP.

    ELSEIF iv_element_id IS NOT INITIAL
    OR iv_type IS NOT INITIAL.

      lv_search = iv_element_id.
      lv_type = iv_type.

    ENDIF.

    DATA(lv_dest) = NEW zcl_se_2parameters( )->read( EXPORTING iv_application           = zif_rs_3constants=>gc_verrechnung_appl
                                                               iv_name                  = zif_rs_3constants=>gc_verrechnung_name
                                                               iv_read_system_dependent = zif_rs_3constants=>gc_verrechnung_dep ).

    IF lv_dest IS NOT INITIAL.

      CALL FUNCTION 'Z_PS_TT_KONT_ELEM_SEARCH' DESTINATION lv_dest
        EXPORTING
          iv_searchstring       = lv_search "is a string and must be longer than 2 characters and must not contain “*” or “%”
          iv_contelem_type      = lv_type "is a char1 which can hold “ ” (any type), “F” (internal orders), “K” (const centers), “P” (only PSP elements)
        IMPORTING
          et_kont_elems         = lt_kont "search result
          es_return             = ls_return "error message.
        EXCEPTIONS
          system_failure        = 1
          communication_failure = 2
          OTHERS                = 3.

      IF sy-subrc NE 0.

        CASE sy-subrc.

          WHEN 1.

            MESSAGE e029(zrs_3) INTO DATA(lv_message).
            raise_error( is_message = lv_message ).

            et_return = VALUE #( (
                      type = zif_rs_3constants=>gc_message_type_error
                      id = zif_rs_3constants=>gc_message_class
                      number = '029' "System Failure during RFC call
                      ) ).

          WHEN 2.

            MESSAGE e030(zrs_3) INTO lv_message.
            raise_error( is_message = lv_message ).

            et_return = VALUE #( (
                      type = zif_rs_3constants=>gc_message_type_error
                      id = zif_rs_3constants=>gc_message_class
                      number = '030' "Communication Error during RFC call
                      ) ).

          WHEN OTHERS.

            MESSAGE e031(zrs_3) INTO lv_message.
            raise_error( is_message = lv_message ).

            et_return = VALUE #( (
                      type = zif_rs_3constants=>gc_message_type_error
                      id = zif_rs_3constants=>gc_message_class
                      number = '031' "Exception during RFC call
                      ) ).

        ENDCASE.

      ELSE.

        IF ls_return-type CA 'EAX'.

          et_return = VALUE #( (
                      type = zif_rs_3constants=>gc_message_type_error
                      id = zif_rs_3constants=>gc_message_class
                      number = '000'
                      message = ls_return-message
                      ) ).

          RETURN.

        ENDIF.

        MOVE lt_kont[] TO et_entityset[].

      ENDIF.

    ENDIF.

  ENDMETHOD.


  METHOD get_acct_elem_type_text.
*----------------------------------------------------------------------*
* SBB AG, SAP CCC-IT, Bern/Berne/Berna
*----------------------------------------------------------------------*
*
*-----------Änderungsdokumentation / documentation of changes----------*
* Version SAP-Rel Datum Bearbeiter
* Beschreibung Get accounting element type text
*----------------------------------------------------------------------*
* >000< 7.50 28.10.2020 Firma(DXC Technology) / Name(Shernan Bamba)
* Programmerstellung / creation of program
*----------------------------------------------------------------------*
* >001< 7.50 tt.mm.jjjj Firma(company) / Name(name)
* ...
*----------------------------------------------------------------------*

    DATA:
      lt_domain_values TYPE STANDARD TABLE OF zrs_3tt_domainwert_s.

*"Get domain values of ZRS_3TT_DO_TYPVERCHEL
    FREE lt_domain_values.

    get_domain_values(
      EXPORTING
        iv_dom_name   = 'ZRS_3TT_DO_TYPVERCHEL'
        iv_langu      = sy-langu
        iv_bool       = 'X'
      IMPORTING
        et_dom_values = lt_domain_values ).

    IF lt_domain_values[] IS NOT INITIAL.

      READ TABLE lt_domain_values
        INTO DATA(ls_domain_values)
        WITH KEY code = iv_acct_elem_type.

      IF sy-subrc EQ 0.

        ev_text = ls_domain_values-beschreibung.

      ENDIF.

    ENDIF.

  ENDMETHOD.


  METHOD get_all_projects.
*----------------------------------------------------------------------*
* SBB AG, SAP CCC-IT, Bern/Berne/Berna
*----------------------------------------------------------------------*
*
*-----------Änderungsdokumentation / documentation of changes----------*
* Version SAP-Rel Datum Bearbeiter
* Beschreibung Get all projects for project overview UI
*----------------------------------------------------------------------*
* >000< 7.50 28.10.2020 Firma(DXC Technology) / Name(Shernan Bamba)
* Programmerstellung / creation of program
*----------------------------------------------------------------------*
* >001< 7.50 tt.mm.jjjj Firma(company) / Name(name)
* ...
*----------------------------------------------------------------------*

    DATA: lt_vorhaben           TYPE zrs_3tt_vorhaben_t,
          lt_vorhaben_c         TYPE zrs_3tt_vorhaben_t,
          ls_vorhaben           TYPE zrs_3tt_vorhaben_s,
          lt_vorhaben_text      TYPE STANDARD TABLE OF zrs_3tt_vorhaben_s,
          lv_fullname           TYPE bapiaddr3-fullname,
          lv_username           TYPE bapibname-bapibname,
          lv_statusbeschreibung TYPE zrs_p_3tt_prjabr-statusbeschreibung.

    CHECK gs_role IS NOT INITIAL.

*    IF iv_condition IS NOT INITIAL.
*
*      SELECT *
*        FROM zrs_3tt_vorhaben
*        INTO TABLE lt_vorhaben_c
*       WHERE (iv_condition)."#EC CI_DYNWHERE
*
*      IF sy-subrc EQ 0.
*
*        SELECT DISTINCT vorhabensnummer
*          FROM zrs_3tt_zuvoranw
*          INTO TABLE @DATA(lt_vorhabensnummer)
*           FOR ALL ENTRIES IN @lt_vorhaben_c
*         WHERE userid = @sy-uname
*           AND vorhabensnummer = @lt_vorhaben_c-vorhabensnummer.
*
*        IF sy-subrc EQ 0.
*
*          MOVE lt_vorhaben_c[] TO lt_vorhaben[].
*
*          LOOP AT lt_vorhaben_c
*            ASSIGNING FIELD-SYMBOL(<fs_vorhaben>).
*
*            READ TABLE lt_vorhabensnummer
*              WITH KEY vorhabensnummer = <fs_vorhaben>-vorhabensnummer
*              TRANSPORTING NO FIELDS.
*
*            IF sy-subrc NE 0.
*
*              DELETE lt_vorhaben
*                WHERE vorhabensnummer = <fs_vorhaben>-vorhabensnummer.
*
*            ENDIF.
*
*          ENDLOOP.
*
*        ENDIF.
*
*      ENDIF.
*
*    ELSE.
**"No filter
*      IF ms_role-ist_sbb_service_management NE 'X'.
*
*        SELECT DISTINCT vorhabensnummer
*          FROM zrs_3tt_zuvoranw
*          INTO TABLE lt_vorhabensnummer
*         WHERE userid = sy-uname. "#EC CI_NOFIRST
*
*        CHECK lt_vorhabensnummer IS NOT INITIAL
*        AND sy-subrc EQ 0.
*
*        SELECT *
*          FROM zrs_3tt_vorhaben
*          INTO TABLE lt_vorhaben
*          FOR ALL ENTRIES IN lt_vorhabensnummer
*         WHERE vorhabensnummer EQ lt_vorhabensnummer-vorhabensnummer.
*
*        IF sy-subrc NE 0.
*
*          FREE lt_vorhaben.
*
*        ENDIF.
*
*      ELSE.
*
**"SBB Manager
*        SELECT *
*          FROM zrs_3tt_vorhaben
*          INTO TABLE lt_vorhaben."#EC CI_NOWHERE
*
*        IF sy-subrc NE 0.
*
*          FREE lt_vorhaben.
*
*        ENDIF.
*
*      ENDIF.
*
*    ENDIF.

    IF iv_condition IS INITIAL.

      SELECT *
        FROM zrs_3tt_vorhaben
        INTO TABLE lt_vorhaben.                         "#EC CI_NOWHERE

      IF sy-subrc NE 0.

        FREE lt_vorhaben.

      ENDIF.

    ELSE.

      SELECT *
        FROM zrs_3tt_vorhaben
        INTO TABLE lt_vorhaben
       WHERE (iv_condition).                           "#EC CI_DYNWHERE

      IF sy-subrc NE 0.

        FREE lt_vorhaben.

      ENDIF.

    ENDIF.

    IF lt_vorhaben[] IS NOT INITIAL.

      IF gs_role-role NE zif_rs_3constants=>gc_role_smsbb.

        SELECT DISTINCT *
          FROM zrs_3tt_zuvoranw
          INTO TABLE @DATA(lt_zuvoranw)
           FOR ALL ENTRIES IN @lt_vorhaben
         WHERE userid = @sy-uname
           AND vorhabensnummer EQ @lt_vorhaben-vorhabensnummer
           AND ( gueltig_ab LE @sy-datum
           AND gueltig_bis  GE @sy-datum ).

        IF sy-subrc NE 0.

          FREE lt_zuvoranw.

        ENDIF.

      ENDIF.

      LOOP AT lt_vorhaben
        ASSIGNING FIELD-SYMBOL(<fs_vorhaben>).

        CLEAR ls_vorhaben.

        MOVE-CORRESPONDING  <fs_vorhaben> TO ls_vorhaben.

        IF gs_role-role EQ zif_rs_3constants=>gc_role_smsbb
        OR gs_role-role EQ zif_rs_3constants=>gc_role_dxcrun.

          ls_vorhaben-editable = abap_true.

        ELSE.

          READ TABLE lt_zuvoranw
            INTO DATA(ls_zuvoranw)
            WITH KEY vorhabensnummer = <fs_vorhaben>-vorhabensnummer.

          IF sy-subrc EQ 0.

            ls_vorhaben-editable = abap_true.

          ENDIF.

        ENDIF.
*"Get Service SPOC Name
        CLEAR: lv_fullname,
               lv_username.

        lv_username = <fs_vorhaben>-service_spoc.

        get_name(
          EXPORTING
            iv_username = lv_username
          IMPORTING
            ev_fullname = lv_fullname ).

        IF lv_fullname IS NOT INITIAL.

          ls_vorhaben-service_spoc_t = lv_fullname.

        ENDIF.

*"Get STV Service SPOC Name
        CLEAR: lv_fullname,
               lv_username.

        lv_username = <fs_vorhaben>-stv_sspoc.

        get_name(
          EXPORTING
            iv_username = lv_username
          IMPORTING
            ev_fullname = lv_fullname ).

        IF lv_fullname IS NOT INITIAL.

          ls_vorhaben-stv_sspoc_t = lv_fullname.

        ENDIF.

*"Get PROJEKTLEITER Name
        CLEAR: lv_fullname,
               lv_username.

        lv_username = <fs_vorhaben>-projektleiter.

        get_name(
          EXPORTING
            iv_username = lv_username
          IMPORTING
            ev_fullname = lv_fullname ).

        IF lv_fullname IS NOT INITIAL.

          ls_vorhaben-projektleiter_t = lv_fullname.

        ENDIF.

*"Get STV PROJEKTLEITER Name
        CLEAR: lv_fullname,
               lv_username.

        lv_username = <fs_vorhaben>-stv_projektleiter.

        get_name(
          EXPORTING
            iv_username = lv_username
          IMPORTING
            ev_fullname = lv_fullname ).

        IF lv_fullname IS NOT INITIAL.

          ls_vorhaben-stv_projektleiter_t = lv_fullname.

        ENDIF.

*"Get SOFTWARE_ARCHITEKT Name
        CLEAR: lv_fullname,
               lv_username.

        lv_username = <fs_vorhaben>-software_architekt.

        get_name(
          EXPORTING
            iv_username = lv_username
          IMPORTING
            ev_fullname = lv_fullname ).

        IF lv_fullname IS NOT INITIAL.

          ls_vorhaben-software_architekt_t = lv_fullname.

        ENDIF.

*"Get STV SOFTWARE_ARCHITEKT Name
        CLEAR: lv_fullname,
               lv_username.

        lv_username = <fs_vorhaben>-stv_swa.

        get_name(
          EXPORTING
            iv_username = lv_username
          IMPORTING
            ev_fullname = lv_fullname ).

        IF lv_fullname IS NOT INITIAL.

          ls_vorhaben-stv_swa_t = lv_fullname.

        ENDIF.

*"Get Status Text
        CLEAR lv_statusbeschreibung.

        get_status_text(
          EXPORTING
            iv_sprache            = sy-langu
            iv_prozessart         = 'V'
            iv_status             = <fs_vorhaben>-status
          IMPORTING
            ev_statusbeschreibung = lv_statusbeschreibung ).

        IF lv_statusbeschreibung IS NOT INITIAL.

          ls_vorhaben-status_t = lv_statusbeschreibung.

        ENDIF.

*"Get Accounting element type text
        get_acct_elem_type_text(
          EXPORTING
            iv_acct_elem_type = <fs_vorhaben>-typ_verrechnungselement
          IMPORTING
            ev_text           = ls_vorhaben-typ_verrechnungselement_t ).

        APPEND ls_vorhaben TO lt_vorhaben_text.

      ENDLOOP.

*"Forecast Check
      check_forecast( CHANGING it_project = lt_vorhaben_text ).

    ENDIF.

    MOVE lt_vorhaben_text[] TO et_vorhaben[].

  ENDMETHOD.


  METHOD get_all_projects_assigned.
*----------------------------------------------------------------------*
* SBB AG, SAP CCC-IT, Bern/Berne/Berna
*----------------------------------------------------------------------*
*
*-----------Änderungsdokumentation / documentation of changes----------*
* Version SAP-Rel Datum Bearbeiter
* Beschreibung Get all projects for project overview UI
*----------------------------------------------------------------------*
* >000< 7.50 28.10.2020 Firma(DXC Technology) / Name(Shernan Bamba)
* Programmerstellung / creation of program
*----------------------------------------------------------------------*
* >001< 7.50 tt.mm.jjjj Firma(company) / Name(name)
* ...
*----------------------------------------------------------------------*

    DATA: lt_vorhaben      TYPE zrs_3tt_vorhaben_t,
          ls_vorhaben      TYPE zrs_3tt_vorhaben_s,
          lt_vorhaben_text TYPE STANDARD TABLE OF zrs_3tt_vorhaben_s,
          lv_fullname      TYPE bapiaddr3-fullname,
          lv_username      TYPE bapibname-bapibname.

    SELECT DISTINCT vorhabensnummer
      FROM zrs_3tt_zuvoranw
      INTO TABLE @DATA(lt_vorhabensnummer)
     WHERE userid = @sy-uname. "#EC CI_NOFIRST

    CHECK lt_vorhabensnummer IS NOT INITIAL
    AND sy-subrc EQ 0.

    SELECT *
      FROM zrs_3tt_vorhaben
      INTO TABLE lt_vorhaben
      FOR ALL ENTRIES IN lt_vorhabensnummer
     WHERE vorhabensnummer EQ lt_vorhabensnummer-vorhabensnummer.

    IF sy-subrc EQ 0.

      LOOP AT lt_vorhaben
        ASSIGNING FIELD-SYMBOL(<fs_vorhaben>).

        CLEAR ls_vorhaben.

        MOVE-CORRESPONDING  <fs_vorhaben> TO ls_vorhaben.
*"Get Service SPOC Name
        CLEAR: lv_fullname,
               lv_username.

        lv_username = <fs_vorhaben>-service_spoc.

        CALL METHOD get_name
          EXPORTING
            iv_username = lv_username
          IMPORTING
            ev_fullname = lv_fullname.

        IF lv_fullname IS NOT INITIAL.

          ls_vorhaben-service_spoc_t = lv_fullname.

        ENDIF.

*"Get STV Service SPOC Name
        CLEAR: lv_fullname,
               lv_username.

        lv_username = <fs_vorhaben>-stv_sspoc.

        CALL METHOD get_name
          EXPORTING
            iv_username = lv_username
          IMPORTING
            ev_fullname = lv_fullname.

        IF lv_fullname IS NOT INITIAL.

          ls_vorhaben-stv_sspoc_t = lv_fullname.

        ENDIF.

*"Get PROJEKTLEITER Name
        CLEAR: lv_fullname,
               lv_username.

        lv_username = <fs_vorhaben>-projektleiter.

        CALL METHOD get_name
          EXPORTING
            iv_username = lv_username
          IMPORTING
            ev_fullname = lv_fullname.

        IF lv_fullname IS NOT INITIAL.

          ls_vorhaben-projektleiter_t = lv_fullname.

        ENDIF.

*"Get STV PROJEKTLEITER Name
        CLEAR: lv_fullname,
               lv_username.

        lv_username = <fs_vorhaben>-stv_projektleiter.

        CALL METHOD get_name
          EXPORTING
            iv_username = lv_username
          IMPORTING
            ev_fullname = lv_fullname.

        IF lv_fullname IS NOT INITIAL.

          ls_vorhaben-stv_projektleiter_t = lv_fullname.

        ENDIF.

*"Get SOFTWARE_ARCHITEKT Name
        CLEAR: lv_fullname,
               lv_username.

        lv_username = <fs_vorhaben>-software_architekt.

        CALL METHOD get_name
          EXPORTING
            iv_username = lv_username
          IMPORTING
            ev_fullname = lv_fullname.

        IF lv_fullname IS NOT INITIAL.

          ls_vorhaben-software_architekt_t = lv_fullname.

        ENDIF.

*"Get STV SOFTWARE_ARCHITEKT Name
        CLEAR: lv_fullname,
               lv_username.

        lv_username = <fs_vorhaben>-stv_swa.

        CALL METHOD get_name
          EXPORTING
            iv_username = lv_username
          IMPORTING
            ev_fullname = lv_fullname.

        IF lv_fullname IS NOT INITIAL.

          ls_vorhaben-stv_swa_t = lv_fullname.

        ENDIF.

        APPEND ls_vorhaben TO lt_vorhaben_text.

      ENDLOOP.

    ENDIF.

    MOVE lt_vorhaben_text[] TO et_vorhaben[].

  ENDMETHOD.


  METHOD get_all_status.
*----------------------------------------------------------------------*
* SBB AG, SAP CCC-IT, Bern/Berne/Berna
*----------------------------------------------------------------------*
*
*-----------Änderungsdokumentation / documentation of changes----------*
* Version SAP-Rel Datum Bearbeiter
* Beschreibung Get all project status
*----------------------------------------------------------------------*
* >000< 7.50 28.10.2020 Firma(DXC Technology) / Name(Shernan Bamba)
* Programmerstellung / creation of program
*----------------------------------------------------------------------*
* >001< 7.50 tt.mm.jjjj Firma(company) / Name(name)
* ...
*----------------------------------------------------------------------*

    IF gs_role-role EQ zif_rs_3constants=>gc_role_smsbb. "Super User

      CALL METHOD zcl_rs_3vorhaben=>get_status
        EXPORTING
          iv_sprache     = iv_sprache
          iv_prozessart  = iv_prozessart
        IMPORTING
          et_status_list = et_status_list.

    ELSEIF iv_status IS NOT INITIAL.

      SELECT *
        FROM zrs_d_3tt_prjabr
        INTO TABLE @DATA(lt_status)
       WHERE sprache = @iv_sprache
         AND prozessart = @iv_prozessart.

      IF sy-subrc EQ 0.

        READ TABLE lt_status
          INTO DATA(ls_current_status)
          WITH KEY estat = iv_status.

        IF sy-subrc EQ 0.

          LOOP AT lt_status
            ASSIGNING FIELD-SYMBOL(<fs_status>)
            WHERE status GE ls_current_status-nsonr
            AND   status LE ls_current_status-hsonr.

            APPEND <fs_status> TO et_status_list.

          ENDLOOP.

        ENDIF.

      ENDIF.

    ELSE.
*"Get all status if no filters
      SELECT *
        FROM zrs_d_3tt_prjabr
        INTO TABLE lt_status
       WHERE sprache = iv_sprache
         AND prozessart = iv_prozessart.

      IF sy-subrc EQ 0.

        MOVE lt_status[] TO et_status_list.

      ENDIF.

    ENDIF.

  ENDMETHOD.


  METHOD get_data.
*----------------------------------------------------------------------*
* SBB AG, SAP CCC-IT, Bern/Berne/Berna
*----------------------------------------------------------------------*
*
*-----------Änderungsdokumentation / documentation of changes----------*
* Version SAP-Rel Datum Bearbeiter
* Beschreibung Get data
*----------------------------------------------------------------------*
* >000< 7.50 28.10.2020 Firma(DXC Technology) / Name(Shernan Bamba)
* Programmerstellung / creation of program
*----------------------------------------------------------------------*
* >001< 7.50 tt.mm.jjjj Firma(company) / Name(name)
* ...
*----------------------------------------------------------------------*

    DATA(lv_condition) = gs_params-io_tech_request_context_query->get_osql_where_clause( ).

    get_all_projects(
      EXPORTING
        iv_condition = lv_condition
      IMPORTING
        et_vorhaben = gs_params-et_entityset ).

  ENDMETHOD.


  METHOD get_domain_values.
*----------------------------------------------------------------------*
* SBB AG, SAP CCC-IT, Bern/Berne/Berna
*----------------------------------------------------------------------*
*
*-----------Änderungsdokumentation / documentation of changes----------*
* Version SAP-Rel Datum Bearbeiter
* Beschreibung Get values given a domain
*----------------------------------------------------------------------*
* >000< 7.50 28.10.2020 Firma(DXC Technology) / Name(Shernan Bamba)
* Programmerstellung / creation of program
*----------------------------------------------------------------------*
* >001< 7.50 tt.mm.jjjj Firma(company) / Name(name)
* ...
*----------------------------------------------------------------------*

    DATA:
      lt_dom_values TYPE STANDARD TABLE OF dd07v,
      ls_dom_values LIKE LINE OF et_dom_values.

    CALL FUNCTION 'DD_DOMVALUES_GET'
      EXPORTING
        domname        = iv_dom_name
        text           = iv_bool
        langu          = iv_langu
      TABLES
        dd07v_tab      = lt_dom_values
      EXCEPTIONS
        wrong_textflag = 1
        OTHERS         = 2.

    IF sy-subrc NE 0.

      FREE et_dom_values.

    ELSE.

      LOOP AT lt_dom_values
        ASSIGNING FIELD-SYMBOL(<fs_dom_values>).

        CLEAR ls_dom_values.

        ls_dom_values-code = <fs_dom_values>-domvalue_l.
        ls_dom_values-beschreibung = <fs_dom_values>-ddtext.

        APPEND ls_dom_values TO et_dom_values.

      ENDLOOP.

    ENDIF.

  ENDMETHOD.


  method GET_EMAIL_ADDRESS.
*----------------------------------------------------------------------*
* SBB AG, SAP CCC-IT, Bern/Berne/Berna
*----------------------------------------------------------------------*
*
*-----------Änderungsdokumentation / documentation of changes----------*
* Version SAP-Rel Datum Bearbeiter
* Beschreibung Get Email address of a user
*----------------------------------------------------------------------*
* >000< 7.50 28.10.2020 Firma(DXC Technology) / Name(Shernan Bamba)
* Programmerstellung / creation of program
*----------------------------------------------------------------------*
* >001< 7.50 tt.mm.jjjj Firma(company) / Name(name)
* ...
*----------------------------------------------------------------------*

    DATA:
      ls_address TYPE bapiaddr3,
      lt_return TYPE bapiret2_t.

    CALL FUNCTION 'BAPI_USER_GET_DETAIL'
      EXPORTING
        username = iv_userid
      IMPORTING
        address  = ls_address
      TABLES
        return   = lt_return.

    IF sy-subrc EQ 0.

      rv_email = ls_address-e_mail.

    ENDIF.

  endmethod.


  METHOD get_name.
*----------------------------------------------------------------------*
* SBB AG, SAP CCC-IT, Bern/Berne/Berna
*----------------------------------------------------------------------*
*
*-----------Änderungsdokumentation / documentation of changes----------*
* Version SAP-Rel Datum Bearbeiter
* Beschreibung Get fullname
*----------------------------------------------------------------------*
* >000< 7.50 28.10.2020 Firma(DXC Technology) / Name(Shernan Bamba)
* Programmerstellung / creation of program
*----------------------------------------------------------------------*
* >001< 7.50 tt.mm.jjjj Firma(company) / Name(name)
* ...
*----------------------------------------------------------------------*

    DATA:
      lt_bapiret2 TYPE STANDARD TABLE OF bapiret2,
      ls_address   TYPE bapiaddr3.

    CALL FUNCTION 'BAPI_USER_GET_DETAIL'
      EXPORTING
        username = iv_username
      IMPORTING
        address  = ls_address
      TABLES
        return   = lt_bapiret2.

    IF ls_address IS NOT INITIAL.

      ev_fullname = ls_address-fullname.
      ev_firstname = ls_address-firstname.
      ev_lastname = ls_address-lastname.

    ENDIF.

  ENDMETHOD.


  METHOD get_project_details.
*----------------------------------------------------------------------*
* SBB AG, SAP CCC-IT, Bern/Berne/Berna
*----------------------------------------------------------------------*
*
*-----------Änderungsdokumentation / documentation of changes----------*
* Version SAP-Rel Datum Bearbeiter
* Beschreibung Get single project utility class
*----------------------------------------------------------------------*
* >000< 7.50 28.10.2020 Firma(DXC Technology) / Name(Shernan Bamba)
* Programmerstellung / creation of program
*----------------------------------------------------------------------*
* >001< 7.50 tt.mm.jjjj Firma(company) / Name(name)
* ...
*----------------------------------------------------------------------*

    DATA:
      lv_fullname           TYPE bapiaddr3-fullname,
      lv_username           TYPE bapibname-bapibname,
      ls_vorhaben_text      TYPE zrs_3tt_vorhaben_s,
      lv_statusbeschreibung TYPE zrs_p_3tt_prjabr-statusbeschreibung.

    SELECT SINGLE *
      FROM zrs_3tt_vorhaben
      INTO @DATA(ls_vorhaben)
     WHERE vorhabensnummer EQ @iv_vorhabensnummer.

    IF sy-subrc EQ 0.

*"Get Service SPOC Name
      CLEAR: lv_fullname,
             lv_username.

      MOVE-CORRESPONDING ls_vorhaben TO ls_vorhaben_text.

      IF gs_role-role EQ zif_rs_3constants=>gc_role_smsbb
      OR gs_role-role EQ zif_rs_3constants=>gc_role_dxcrun.

        ls_vorhaben_text-editable = abap_true.

      ELSE.

        SELECT COUNT( * )
          FROM zrs_3tt_zuvoranw
          INTO @DATA(ls_count)
         WHERE userid = @sy-uname
           AND vorhabensnummer EQ @ls_vorhaben-vorhabensnummer
           AND ( gueltig_ab LE @sy-datum
           AND gueltig_bis  GE @sy-datum ).

        IF sy-subrc EQ 0
        AND ls_count IS NOT INITIAL.

          ls_vorhaben_text-editable = abap_true.

        ENDIF.

      ENDIF.

      lv_username = ls_vorhaben-service_spoc.

      CALL METHOD get_name
        EXPORTING
          iv_username = lv_username
        IMPORTING
          ev_fullname = lv_fullname.

      IF lv_fullname IS NOT INITIAL.

        ls_vorhaben_text-service_spoc_t = lv_fullname.

      ENDIF.

*"Get STV Service SPOC Name
      CLEAR: lv_fullname,
             lv_username.

      lv_username = ls_vorhaben-stv_sspoc.

      CALL METHOD get_name
        EXPORTING
          iv_username = lv_username
        IMPORTING
          ev_fullname = lv_fullname.

      IF lv_fullname IS NOT INITIAL.

        ls_vorhaben_text-stv_sspoc_t = lv_fullname.

      ENDIF.

*"Get PROJEKTLEITER Name
      CLEAR: lv_fullname,
             lv_username.

      lv_username = ls_vorhaben-projektleiter.

      CALL METHOD get_name
        EXPORTING
          iv_username = lv_username
        IMPORTING
          ev_fullname = lv_fullname.

      IF lv_fullname IS NOT INITIAL.

        ls_vorhaben_text-projektleiter_t = lv_fullname.

      ENDIF.

*"Get STV PROJEKTLEITER Name
      CLEAR: lv_fullname,
             lv_username.

      lv_username = ls_vorhaben-stv_projektleiter.

      CALL METHOD get_name
        EXPORTING
          iv_username = lv_username
        IMPORTING
          ev_fullname = lv_fullname.

      IF lv_fullname IS NOT INITIAL.

        ls_vorhaben_text-stv_projektleiter_t = lv_fullname.

      ENDIF.

*"Get SOFTWARE_ARCHITEKT Name
      CLEAR: lv_fullname,
             lv_username.

      lv_username = ls_vorhaben-software_architekt.

      CALL METHOD get_name
        EXPORTING
          iv_username = lv_username
        IMPORTING
          ev_fullname = lv_fullname.

      IF lv_fullname IS NOT INITIAL.

        ls_vorhaben_text-software_architekt_t = lv_fullname.

      ENDIF.

*"Get STV SOFTWARE_ARCHITEKT Name
      CLEAR: lv_fullname,
             lv_username.

      lv_username = ls_vorhaben-stv_swa.

      CALL METHOD get_name
        EXPORTING
          iv_username = lv_username
        IMPORTING
          ev_fullname = lv_fullname.

      IF lv_fullname IS NOT INITIAL.

        ls_vorhaben_text-stv_swa_t = lv_fullname.

      ENDIF.

*"Get Status text
      CLEAR lv_statusbeschreibung.

      CALL METHOD get_status_text
        EXPORTING
          iv_sprache            = sy-langu
          iv_prozessart         = 'V'
          iv_status             = ls_vorhaben-status
        IMPORTING
          ev_statusbeschreibung = lv_statusbeschreibung.

      IF lv_statusbeschreibung IS NOT INITIAL.

        ls_vorhaben_text-status_t = lv_statusbeschreibung.

      ENDIF.

*"Get Accounting element type text
      CALL METHOD get_acct_elem_type_text
        EXPORTING
          iv_acct_elem_type = ls_vorhaben-typ_verrechnungselement
        IMPORTING
          ev_text           = ls_vorhaben_text-typ_verrechnungselement_t.

*"Forecast Check
      ls_vorhaben_text-forecast_maint = check_forecast( EXPORTING is_project = ls_vorhaben_text ).

      MOVE ls_vorhaben_text TO es_vorhaben.

    ENDIF.

  ENDMETHOD.


  METHOD get_role.
*----------------------------------------------------------------------*
* SBB AG, SAP CCC-IT, Bern/Berne/Berna
*----------------------------------------------------------------------*
*
*-----------Änderungsdokumentation / documentation of changes----------*
* Version SAP-Rel Datum Bearbeiter
* Beschreibung Get Role Method
*----------------------------------------------------------------------*
* >000< 7.50 28.10.2020 Firma(DXC Technology) / Name(Shernan Bamba)
* Programmerstellung / creation of program
*----------------------------------------------------------------------*
* >001< 7.50 tt.mm.jjjj Firma(company) / Name(name)
* ...
*----------------------------------------------------------------------*

    DATA: lv_userid TYPE uname.

    IF iv_userid IS NOT INITIAL.

      lv_userid = iv_userid.

    ELSE.

      IF line_exists( it_keys[ name = 'USERID' ] ).

        TRY.
            DATA(ls_keys) = it_keys[ name = 'USERID' ].

          CATCH cx_sy_itab_line_not_found.

        ENDTRY.

      ENDIF.

      IF sy-subrc NE 0.

        lv_userid = sy-uname.

      ELSE.

        IF ls_keys-value EQ 'X'.

          lv_userid = sy-uname.

        ELSE.

          lv_userid = ls_keys-value.

        ENDIF.

      ENDIF.

    ENDIF.

    CALL FUNCTION 'Z_RS_3GET_ROLES'
      EXPORTING
        iv_user   = lv_userid
      IMPORTING
        et_return = et_return
        et_roles  = et_roles.

    IF sy-subrc EQ 0.

      LOOP AT et_roles
        ASSIGNING FIELD-SYMBOL(<fs_roles>).

        CASE <fs_roles>-zrs_ttool.

          WHEN 'DSPOC'.

            es_entity-ist_dev_spoc = 'X'.

            IF es_entity-role IS INITIAL
            OR es_entity-role NE 'SMSBB'
            OR es_entity-role NE 'PLSWA'.

              es_entity-role = <fs_roles>-zrs_ttool.

            ENDIF.

          WHEN 'DXRUN'.

            es_entity-ist_dxc_run = 'X'.

            IF es_entity-role IS INITIAL
            OR es_entity-role NE 'SMSBB'
            OR es_entity-role NE 'PLSWA'
            OR es_entity-role NE 'DSPOC'.

              es_entity-role = <fs_roles>-zrs_ttool.

            ENDIF.

          WHEN 'PLSWA'.

            es_entity-ist_projektleiter_swa = 'X'.

            IF es_entity-role IS INITIAL
            OR es_entity-role NE 'SMSBB'.

              es_entity-role = <fs_roles>-zrs_ttool.

            ENDIF.

          WHEN 'SMSBB'.

            es_entity-ist_sbb_service_management = 'X'.

            es_entity-role = <fs_roles>-zrs_ttool.

        ENDCASE.

      ENDLOOP.
*      es_entity = at_role.
      es_entity-userid = gs_role-userid.
      es_entity-fullname = gs_role-fullname.
      es_entity-firstname = gs_role-firstname.
      es_entity-lastname = gs_role-lastname.

    ENDIF.

  ENDMETHOD.


  METHOD get_status.
*----------------------------------------------------------------------*
* SBB AG, SAP CCC-IT, Bern/Berne/Berna
*----------------------------------------------------------------------*
*
*-----------Änderungsdokumentation / documentation of changes----------*
* Version SAP-Rel Datum Bearbeiter
* Beschreibung Get all project status
*----------------------------------------------------------------------*
* >000< 7.50 28.10.2020 Firma(DXC Technology) / Name(Shernan Bamba)
* Programmerstellung / creation of program
*----------------------------------------------------------------------*
* >001< 7.50 tt.mm.jjjj Firma(company) / Name(name)
* ...
*----------------------------------------------------------------------*

    SELECT *
      FROM zrs_d_3tt_prjabr
      INTO TABLE et_status_list
     WHERE sprache = iv_sprache
       AND prozessart = iv_prozessart.

    IF sy-subrc NE 0.

      FREE et_status_list.

    ENDIF.

  ENDMETHOD.


  METHOD get_status_text.
*----------------------------------------------------------------------*
* SBB AG, SAP CCC-IT, Bern/Berne/Berna
*----------------------------------------------------------------------*
*
*-----------Änderungsdokumentation / documentation of changes----------*
* Version SAP-Rel Datum Bearbeiter
* Beschreibung Get status description
*----------------------------------------------------------------------*
* >000< 7.50 28.10.2020 Firma(DXC Technology) / Name(Shernan Bamba)
* Programmerstellung / creation of program
*----------------------------------------------------------------------*
* >001< 7.50 tt.mm.jjjj Firma(company) / Name(name)
* ...
*----------------------------------------------------------------------*

    SELECT SINGLE statusbeschreibung
      FROM zrs_d_3tt_prjabr
      INTO ev_statusbeschreibung
     WHERE sprache = iv_sprache
       AND prozessart = iv_prozessart
       AND estat = iv_status.

    IF sy-subrc NE 0.

      CLEAR ev_statusbeschreibung.

    ENDIF.

  ENDMETHOD.


  METHOD get_user_list.
*----------------------------------------------------------------------*
* SBB AG, SAP CCC-IT, Bern/Berne/Berna
*----------------------------------------------------------------------*
*
*-----------Änderungsdokumentation / documentation of changes----------*
* Version SAP-Rel Datum Bearbeiter
* Beschreibung Get user list for project
*----------------------------------------------------------------------*
* >000< 7.50 28.10.2020 Firma(DXC Technology) / Name(Shernan Bamba)
* Programmerstellung / creation of program
*----------------------------------------------------------------------*
* >001< 7.50 tt.mm.jjjj Firma(company) / Name(name)
* ...
*----------------------------------------------------------------------*

    TYPES:
      BEGIN OF ts_users,
        uname      TYPE agr_users-uname,
        name_first TYPE v_usr_name-name_first,
        name_last  TYPE v_usr_name-name_last,
        name_text  TYPE v_usr_name-name_text,
      END OF ts_users.

    DATA:
      lt_users    TYPE STANDARD TABLE OF ts_users,
      lv_type     TYPE zrs_3tt_bapiusname_s-type,
      lv_agr_name TYPE agr_users-agr_name.

    IF it_filter[] IS NOT INITIAL.

      DATA(lt_property) = it_filter[ property = 'TYPE' ].

      DATA(lt_filter) = lt_property-select_options[ 1 ].

      lv_type = lt_filter-low.

    ENDIF.

    CASE lv_type.

      WHEN zif_rs_3constants=>gc_role_smsbb.

        lv_agr_name = 'Z3_CA_XXXX_ESN_MAU_TRITOOLP008'.

      WHEN zif_rs_3constants=>gc_role_plswa.

        lv_agr_name = 'Z3_CA_XXXX_ESN_MAU_TRITOOLP004'.

      WHEN zif_rs_3constants=>gc_role_dxcrun.

        lv_agr_name = 'Z3_CA_XXXX_ESN_MAU_TRITOOLP005'.

      WHEN zif_rs_3constants=>gc_role_devspoc.

        lv_agr_name = 'Z3_CA_XXXX_ESN_MAU_TRITOOLP003'.

      WHEN OTHERS.

        lv_agr_name = 'Z3_CA_XXXX_ESN_MAU_TRITOOLP000'.

    ENDCASE.

    SELECT a~uname
           b~name_first
           b~name_last
           b~name_text
      FROM agr_users AS a
     INNER JOIN v_usr_name AS b ON a~uname = b~bname
      INTO CORRESPONDING FIELDS OF TABLE lt_users
     WHERE a~agr_name EQ 'Z3_CA_XXXX_ESN_MAU_TRITOOLP000'
        OR a~agr_name EQ lv_agr_name.

    IF sy-subrc EQ 0.

      et_users = CORRESPONDING #( lt_users MAPPING username  = uname
                                                   firstname = name_first
                                                   lastname  = name_last
                                                   fullname  = name_text ).

    ENDIF.
*NTO TABLE lt_users
*          FROM v_usr_name AS a
*         INNER JOIN usr02 AS b ON a~bname = b~bname
*         WHERE a~name_text IN lr_fullname
*           AND a~name_first NE space
*           AND b~ustyp EQ 'A'.

*    DATA:
*      lt_selection_range TYPE STANDARD TABLE OF bapiussrge,
*      ls_selection_range TYPE bapiussrge,
*      lt_users           TYPE zrs_3tt_bapiusname_t,
*      lt_users2          TYPE zrs_3tt_bapiusname_t,
*      lr_fullname        TYPE RANGE OF v_usr_name-name_text,
*      ls_fullname        LIKE LINE OF lr_fullname,
*      lr_username        TYPE RANGE OF v_usr_name-name_text,
*      ls_username        LIKE LINE OF lr_username,
*      ls_entity          TYPE zrs_3tt_benutzerrollen_s,
*      lt_return        TYPE STANDARD TABLE OF bapiret2,
*      lt_benutzerollen TYPE zrs_3tt_uroles_t.

*    IF it_filter[] IS NOT INITIAL.
*
**"Process Fullname filter
*      READ TABLE it_filter
*        WITH KEY property = 'FULLNAME'
*        TRANSPORTING NO FIELDS.
*
*      IF sy-subrc EQ 0.
*
*        LOOP AT it_filter
*          INTO DATA(ls_filter)
*          WHERE property EQ 'FULLNAME'.
*
*          LOOP AT ls_filter-select_options
*            INTO DATA(ls_select_options).
*
*            MOVE-CORRESPONDING ls_select_options TO ls_fullname.
*
*            APPEND ls_fullname TO lr_fullname.
*
*          ENDLOOP.
*
*        ENDLOOP.
*
*        SELECT a~bname
*               a~name_first
*               a~name_last
*               a~name_text
*          INTO TABLE lt_users
*          FROM v_usr_name AS a
*         INNER JOIN usr02 AS b ON a~bname = b~bname
*         WHERE a~name_text IN lr_fullname
*           AND a~name_first NE space
*           AND b~ustyp EQ 'A'.
*
*        IF sy-subrc EQ 0.
*
*          APPEND LINES OF lt_users TO lt_users2.
*
*        ENDIF.
*
*      ENDIF.
*
**"Process Username Filter
*      READ TABLE it_filter
*        WITH KEY property = 'USERNAME'
*        TRANSPORTING NO FIELDS.
*
*      IF sy-subrc EQ 0.
*
*        LOOP AT it_filter
*          INTO ls_filter
*          WHERE property EQ 'USERNAME'.
*
*          LOOP AT ls_filter-select_options
*            INTO ls_select_options.
*
*            MOVE-CORRESPONDING ls_select_options TO ls_username.
*
*            APPEND ls_username TO lr_username.
*
*          ENDLOOP.
*
*        ENDLOOP.
*
*        SELECT a~bname
*               a~name_first
*               a~name_last
*               a~name_text
*          INTO TABLE et_users
*          FROM v_usr_name AS a
*         INNER JOIN usr02 AS b ON a~bname = b~bname
*         WHERE a~bname IN lr_username
*           AND a~name_first NE space
*           AND b~ustyp EQ 'A'.
*
*        IF sy-subrc EQ 0.
*
*          APPEND LINES OF lt_users TO lt_users2.
*
*        ENDIF.
*
*      ENDIF.
*
*    ELSE.
*
*      SELECT a~bname
*             a~name_first
*             a~name_last
*             a~name_text
*        INTO TABLE lt_users2
*        FROM v_usr_name AS a
*       INNER JOIN usr02 AS b ON a~bname = b~bname
*       WHERE b~ustyp EQ 'A'
*         AND a~name_first NE space.
*
*    ENDIF.

*"Get authorized users only
*    et_users[] = lt_users2[].

*    IF lt_users2[] IS NOT INITIAL.
*
*      LOOP AT lt_users2
*        ASSIGNING FIELD-SYMBOL(<fs_users>).
*
*        CALL METHOD zcl_rs_3vorhaben=>get_role
*          EXPORTING
*            iv_userid = <fs_users>-username
*          IMPORTING
*            et_roles  = lt_benutzerollen
*            et_return = lt_return
*            es_entity = ls_entity.
*
*        IF sy-subrc EQ 0
*        AND lt_benutzerollen IS NOT INITIAL.
*
*          APPEND <fs_users> TO et_users.
*
*        ENDIF.
*
*      ENDLOOP.
*
*    ENDIF.

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


  METHOD send_email.
*----------------------------------------------------------------------*
* SBB AG, SAP CCC-IT, Bern/Berne/Berna
*----------------------------------------------------------------------*
*
*-----------Änderungsdokumentation / documentation of changes----------*
* Version SAP-Rel Datum Bearbeiter
* Beschreibung Send Email
*----------------------------------------------------------------------*
* >000< 7.50 28.10.2020 Firma(DXC Technology) / Name(Shernan Bamba)
* Programmerstellung / creation of program
*----------------------------------------------------------------------*
* >001< 7.50 tt.mm.jjjj Firma(company) / Name(name)
* ...
*----------------------------------------------------------------------*

    TYPES:
      BEGIN OF ts_name_value,
        name  TYPE zse_name,
        value TYPE zse_value,
      END OF ts_name_value .
    DATA:
      lt_name_value TYPE STANDARD TABLE OF ts_name_value.

    MOVE-CORRESPONDING it_name_value[] TO lt_name_value[].


    CALL METHOD zcl_se_2mail_2=>send_mail
      EXPORTING
        iv_mail_id             = iv_mail_id
        iv_sender_address      = iv_sender_address
        iv_sender_address_type = iv_sender_address_type
        iv_language            = iv_language
        iv_send_immediately    = iv_send_immediately
        iv_html                = iv_send_immediately
        iv_subject_as_title    = iv_subject_as_title
        it_name_value          = lt_name_value
        it_recipients          = it_recipients
        it_table               = it_table
        it_text_table          = it_text_table
      IMPORTING
        ev_error_occurred      = ev_error
      CHANGING
        ct_messages            = ct_messages.


  ENDMETHOD.


  METHOD update_project.
*----------------------------------------------------------------------*
* SBB AG, SAP CCC-IT, Bern/Berne/Berna
*----------------------------------------------------------------------*
*
*-----------Änderungsdokumentation / documentation of changes----------*
* Version SAP-Rel Datum Bearbeiter
* Beschreibung Update Project Utility class
*----------------------------------------------------------------------*
* >000< 7.50 28.10.2020 Firma(DXC Technology) / Name(Shernan Bamba)
* Programmerstellung / creation of program
*----------------------------------------------------------------------*
* >001< 7.50 tt.mm.jjjj Firma(company) / Name(name)
* ...
*----------------------------------------------------------------------*
    DATA: lt_zuvoranw       TYPE STANDARD TABLE OF zrs_3tt_zuvoranw,
          ls_zuvoranw       TYPE zrs_3tt_zuvoranw,
          lt_messages       TYPE bapiret2_t,
          lv_mail_error     TYPE flag,
          lt_recipients_dat TYPE STANDARD TABLE OF zse_2_mail_rec,
          lt_recipients_pl  TYPE STANDARD TABLE OF zse_2_mail_rec,
          lt_recipients_for TYPE STANDARD TABLE OF zse_2_mail_rec,
          lt_name_value_dat TYPE STANDARD TABLE OF zse_name_value,
          lt_name_value_for TYPE STANDARD TABLE OF zse_name_value.

    CHECK is_project-vorhabensnummer IS NOT INITIAL.

    DATA(ls_project) = is_project.

    IF  ls_project-projekt_forecast_aw NE 0
    AND ls_project-projekt_forecast_tp NE 0
    AND ls_project-status              EQ zif_rs_3constants=>gc_vorhaben_status-sv0001. "Vorabinfo

      ls_project-status = zif_rs_3constants=>gc_vorhaben_status-sv0002. "In vorbereitung

    ELSEIF  ls_project-projekt_forecast_aw EQ 0
    AND ls_project-projekt_forecast_tp     EQ 0
    AND ls_project-status                  EQ zif_rs_3constants=>gc_vorhaben_status-sv0002.

      ls_project-status = zif_rs_3constants=>gc_vorhaben_status-sv0001. "Advanced info

    ENDIF.

    SELECT SINGLE *
      FROM zrs_3tt_vorhaben
      INTO @DATA(ls_old_project)
     WHERE vorhabensnummer = @ls_project-vorhabensnummer.

    IF sy-subrc EQ 0.
*"Project start/Dev start date email config
      IF ls_old_project-projekt_start      NE ls_project-projekt_start
      OR ls_old_project-projekt_ende       NE ls_project-projekt_ende
      OR ls_old_project-entwicklungsstart  NE ls_project-entwicklungsstart.

        DATA(lv_dat_change) = 'X'.

        DATA(lv_sender_dat) = gs_sender_email.

        lt_name_value_dat = VALUE #( (
                              name = zif_rs_3constants=>gc_email_namevalues_abruf-vorhabensnummer
                              value = ls_project-vorhabensnummer
                              )
                              (
                              name = zif_rs_3constants=>gc_email_namevalues_abruf-projektbezeichnung
                              value = ls_project-projektbezeichnung
                              )
                              (
                              name = zif_rs_3constants=>gc_email_namevalues_abruf-projekt_start
                              value = ls_project-projekt_start
                              )
                              (
                              name = zif_rs_3constants=>gc_email_namevalues_abruf-entwicklungsstart
                              value = ls_project-entwicklungsstart
                              ) ).

        lt_recipients_dat = VALUE #( (
                              address = me->get_email_address( EXPORTING iv_userid =  ls_project-software_architekt )
                              address_type = zif_rs_3constants=>gc_email_kind-address_type_u
                              recepient_type = zif_rs_3constants=>gc_email_kind-recepient_type_to
                              )
                              (
                              address = me->get_email_address( EXPORTING iv_userid =  ls_project-service_spoc )
                              address_type = zif_rs_3constants=>gc_email_kind-address_type_u
                              recepient_type = zif_rs_3constants=>gc_email_kind-recepient_type_to
                              )
                              (
                              address = me->get_email_address( EXPORTING iv_userid =  ls_project-stv_swa )
                              address_type = zif_rs_3constants=>gc_email_kind-address_type_u
                              recepient_type = zif_rs_3constants=>gc_email_kind-recepient_type_cc
                              )
                              (
                              address = me->get_email_address( EXPORTING iv_userid =  ls_project-stv_sspoc )
                              address_type = zif_rs_3constants=>gc_email_kind-address_type_u
                              recepient_type = zif_rs_3constants=>gc_email_kind-recepient_type_cc
                              )
                              ).

        DELETE lt_recipients_dat
         WHERE address = space.

      ENDIF.

*"Forecast Email config
      IF ls_old_project-projekt_forecast_aw NE ls_project-projekt_forecast_aw
      OR ls_old_project-projekt_forecast_tp NE ls_project-projekt_forecast_tp.

        DATA(lv_for_change) = 'X'.

        DATA(lv_sender_for) = zif_rs_3constants=>gc_email_sender-no_reply.

        lt_name_value_for = VALUE #( (
                              name = zif_rs_3constants=>gc_email_namevalues_abruf-vorhabensnummer
                              value = ls_project-vorhabensnummer
                              )
                              (
                              name = zif_rs_3constants=>gc_email_namevalues_abruf-projektbezeichnung
                              value = ls_project-projektbezeichnung
                              ) ).

        lt_recipients_for = VALUE #( (
                          address = me->get_email_address( EXPORTING iv_userid =  ls_project-projektleiter )
                          address_type = zif_rs_3constants=>gc_email_kind-address_type_u
                          recepient_type = zif_rs_3constants=>gc_email_kind-recepient_type_to
                          )
                          (
                          address = me->get_email_address( EXPORTING iv_userid =  ls_project-service_spoc )
                          address_type = zif_rs_3constants=>gc_email_kind-address_type_u
                          recepient_type = zif_rs_3constants=>gc_email_kind-recepient_type_to
                          )
                          (
                          address = me->get_email_address( EXPORTING iv_userid =  ls_project-stv_projektleiter )
                          address_type = zif_rs_3constants=>gc_email_kind-address_type_u
                          recepient_type = zif_rs_3constants=>gc_email_kind-recepient_type_cc
                          )
                          (
                          address = me->get_email_address( EXPORTING iv_userid =  ls_project-stv_sspoc )
                          address_type = zif_rs_3constants=>gc_email_kind-address_type_u
                          recepient_type = zif_rs_3constants=>gc_email_kind-recepient_type_cc
                          )
                          ).

        DELETE lt_recipients_for
         WHERE address = space.

      ENDIF.

    ENDIF.

*"Check accounting element if valid
    get_accounting_element(
      EXPORTING
        iv_element_id = CONV #( ls_project-verrechnungselement )
        iv_type = ls_project-typ_verrechnungselement
      IMPORTING
        et_entityset = DATA(lt_accounting_element)
        et_return = DATA(lt_return_elem)
      ).

    IF  lt_return_elem[] IS NOT INITIAL
    AND lt_accounting_element[] IS INITIAL.

      CLEAR: ls_project-verrechnungselement,
             ls_project-typ_verrechnungselement.

      DATA(lo_message_container) = gs_params-mo_context->get_message_container( ).

      lo_message_container->add_messages_from_bapi(
        EXPORTING
          it_bapi_messages = lt_return_elem
          iv_add_to_response_header = abap_true
          ).

    ENDIF.

*"Update zrs_3tt_vorhaben
    UPDATE zrs_3tt_vorhaben
      SET ppm_nummer              = ls_project-ppm_nummer
          projektleiter           = ls_project-projektleiter
          stv_projektleiter       = ls_project-stv_projektleiter
          software_architekt      = ls_project-software_architekt
          stv_swa                 = ls_project-stv_swa
          projekt_forecast_aw     = ls_project-projekt_forecast_aw
          projekt_forecast_tp     = ls_project-projekt_forecast_tp
          service_spoc            = ls_project-service_spoc
          stv_sspoc               = ls_project-stv_sspoc
          funktionaler_bereich    = ls_project-funktionaler_bereich
          verrechnungselement     = ls_project-verrechnungselement
          typ_verrechnungselement = ls_project-typ_verrechnungselement
          projektbezeichnung      = ls_project-projektbezeichnung
          kommentar               = ls_project-kommentar
          projekt_start           = ls_project-projekt_start
          projekt_ende            = ls_project-projekt_ende
          entwicklungsstart       = ls_project-entwicklungsstart
          status                  = ls_project-status
    WHERE vorhabensnummer         = ls_project-vorhabensnummer.

    IF sy-subrc EQ 0.

*"Save to project assignment table
      lt_zuvoranw = VALUE #( ( mandt = sy-mandt
                               vorhabensnummer = ls_project-vorhabensnummer
                               userid = ls_project-projektleiter "Projektleiter
                               gueltig_ab = sy-datum
                               gueltig_bis = '99991231'
                               zrs_ttool = zif_rs_3constants=>gc_role_plswa
                             )
                             (
                               mandt = sy-mandt
                               vorhabensnummer = ls_project-vorhabensnummer
                               userid = ls_project-stv_projektleiter "STV Projectleiter
                               gueltig_ab = sy-datum
                               gueltig_bis = '99991231'
                               zrs_ttool = zif_rs_3constants=>gc_role_plswa
                             )
                             (
                               mandt = sy-mandt
                               vorhabensnummer = ls_project-vorhabensnummer
                               userid = ls_project-software_architekt "Software Architekt
                               gueltig_ab = sy-datum
                               gueltig_bis = '99991231'
                               zrs_ttool = zif_rs_3constants=>gc_role_plswa
                             )
                             (
                               mandt = sy-mandt
                               vorhabensnummer = ls_project-vorhabensnummer
                               userid = ls_project-stv_swa  "STV SWA
                               gueltig_ab = sy-datum
                               gueltig_bis = '99991231'
                               zrs_ttool = zif_rs_3constants=>gc_role_plswa
                             )
                             (
                               mandt = sy-mandt
                               vorhabensnummer = ls_project-vorhabensnummer
                               userid = ls_project-service_spoc "Service Spoc
                               gueltig_ab = sy-datum
                               gueltig_bis = '99991231'
                               zrs_ttool = zif_rs_3constants=>gc_role_devspoc
                             )
                             (
                               mandt = sy-mandt
                               vorhabensnummer = ls_project-vorhabensnummer
                               userid = ls_project-stv_sspoc "Stv Sspoc
                               gueltig_ab = sy-datum
                               gueltig_bis = '99991231'
                               zrs_ttool = zif_rs_3constants=>gc_role_devspoc
                             ) ).

      SORT lt_zuvoranw.

      DELETE ADJACENT DUPLICATES FROM lt_zuvoranw
        COMPARING ALL FIELDS.

      DELETE lt_zuvoranw
        WHERE userid = space.

      MODIFY zrs_3tt_zuvoranw
        FROM TABLE lt_zuvoranw.

      IF sy-subrc EQ 0.

        et_return = VALUE #( (
                      type = zif_rs_3constants=>gc_message_type_success
                      id = zif_rs_3constants=>gc_message_class
                      number = '004' "Project number &1 updated
                      message = ls_project-vorhabensnummer
                      ) ).

        es_project = CORRESPONDING #( ls_project ).

*"Forecast Check
        es_project-forecast_maint = check_forecast( EXPORTING is_project = es_project ).

*        DATA(lo_entitat) = zcl_rs_3abrufkopf=>create(
*        VALUE zcl_rs_3abrufkopf=>ts_input_parameters(
*            iv_entity_name           = gs_params-iv_entity_name
*            iv_entity_set_name       = gs_params-iv_entity_set_name
*            iv_source_name           = gs_params-iv_source_name
*            er_entity                = VALUE zcl_rs_3abrufkopf=>ts_abrufkopf(
*                                            mandt = sy-mandt
*                                            vorhabensnummer = ls_project-vorhabensnummer
*                                            verrechnungselement = ls_project-verrechnungselement
*                                            typ_verrechnungselement = ls_project-typ_verrechnungselement
*                                            status_abruf = zif_rs_3constants=>gc_abruf_new_status
*                                          )
*                          ) ).
*        lo_entitat->post( ).

*"Send Email project/dev start date change
        IF lv_dat_change IS NOT INITIAL.

          me->send_email(
            EXPORTING
              iv_mail_id             = zif_rs_3constants=>gc_email_templates_vorhaben-et_start_date
              iv_sender_address      = CONV #( lv_sender_dat )
              iv_sender_address_type = 'U'
              iv_language            = sy-langu
              iv_send_immediately    = 'X'
              iv_html                = 'X'
*              iv_subject_as_title    = 'X'
              it_name_value          = lt_name_value_dat
              it_recipients          = lt_recipients_dat
             IMPORTING
              ev_error               = lv_mail_error
            CHANGING
              ct_messages            = lt_messages
              ).
*"create forecast template into forecast table based on
*"project header dates
          CALL METHOD zcl_rs_3vorhabenforecast=>create_forecast
            EXPORTING
              is_project = ls_project.

*          es_project-notify_pl = 'X'.

          lo_message_container = gs_params-mo_context->get_message_container( ).

          lo_message_container->add_message(
            EXPORTING
              iv_msg_type               = /iwbep/cl_cos_logger=>success
              iv_msg_id                 = zif_rs_3constants=>gc_message_class
              iv_msg_number             = '039'
              iv_add_to_response_header = abap_true ).

        ENDIF.

*"Send Email forecast change
        IF lv_for_change IS NOT INITIAL.

          me->send_email(
            EXPORTING
              iv_mail_id             = zif_rs_3constants=>gc_email_templates_vorhaben-et_forecast
              iv_sender_address      = CONV #( lv_sender_for )
              iv_sender_address_type = 'U'
              iv_language            = sy-langu
              iv_send_immediately    = 'X'
              iv_html                = 'X'
              it_name_value          = lt_name_value_for
              it_recipients          = lt_recipients_for
             IMPORTING
              ev_error               = lv_mail_error
            CHANGING
              ct_messages            = lt_messages
              ).

        ENDIF.

      ELSE.

        MESSAGE e005(zrs_3) INTO DATA(lv_message).
        raise_error( is_message = lv_message ).

        et_return = VALUE #( (
                      type = 'E'
                      id = zif_rs_3constants=>gc_message_class
                      number = '005' "Project number &1 update error
                      message = ls_project-vorhabensnummer
                      ) ).

      ENDIF.

    ELSE.

      MESSAGE e005(zrs_3) INTO lv_message.
      raise_error( is_message = lv_message ).

      et_return = VALUE #( (
                    type = 'E'
                    id = zif_rs_3constants=>gc_message_class
                    number = '005' "Project number &1 update error
                    message = ls_project-vorhabensnummer
                    ) ).

    ENDIF.

  ENDMETHOD.


  METHOD verify_user.
*----------------------------------------------------------------------*
* SBB AG, SAP CCC-IT, Bern/Berne/Berna
*----------------------------------------------------------------------*
*
*-----------Änderungsdokumentation / documentation of changes----------*
* Version SAP-Rel Datum Bearbeiter
* Beschreibung Verify User if existing
*----------------------------------------------------------------------*
* >000< 7.50 28.10.2020 Firma(DXC Technology) / Name(Shernan Bamba)
* Programmerstellung / creation of program
*----------------------------------------------------------------------*
* >001< 7.50 tt.mm.jjjj Firma(company) / Name(name)
* ...
*----------------------------------------------------------------------*

*    CONSTANTS c_088 TYPE bapiret2-number VALUE '088'.
*
*    DATA ls_return TYPE  bapiret2.
*
*    CALL FUNCTION 'BAPI_USER_EXISTENCE_CHECK'
*      EXPORTING
*        username = iv_username
*      IMPORTING
*        return   = ls_return.
*
*    IF sy-subrc EQ 0.
*
*      IF ls_return-number EQ c_088. "User exists
*
*        rv_return = 'X'.
*
*      ENDIF.
*
*    ENDIF.

  ENDMETHOD.


  METHOD zif_rs_3dpc_antwort~get_response_post.
*----------------------------------------------------------------------*
* SBB AG, SAP CCC-IT, Bern/Berne/Berna
*----------------------------------------------------------------------*
*
*-----------Änderungsdokumentation / documentation of changes----------*
* Version SAP-Rel Datum Bearbeiter
* Beschreibung Get Response from Create
*----------------------------------------------------------------------*
* >000< 7.50 28.10.2020 Firma(DXC Technology) / Name(Shernan Bamba)
* Programmerstellung / creation of program
*----------------------------------------------------------------------*
* >001< 7.50 tt.mm.jjjj Firma(company) / Name(name)
* ...
*----------------------------------------------------------------------*

    es_entity = gs_params-er_entity.

    es_mo_context = gs_params-mo_context.

  ENDMETHOD.


  METHOD zif_rs_3dpc_antwort~get_response_query.
*----------------------------------------------------------------------*
* SBB AG, SAP CCC-IT, Bern/Berne/Berna
*----------------------------------------------------------------------*
*
*-----------Änderungsdokumentation / documentation of changes----------*
* Version SAP-Rel Datum Bearbeiter
* Beschreibung Get Response from entityset
*----------------------------------------------------------------------*
* >000< 7.50 28.10.2020 Firma(DXC Technology) / Name(Shernan Bamba)
* Programmerstellung / creation of program
*----------------------------------------------------------------------*
* >001< 7.50 tt.mm.jjjj Firma(company) / Name(name)
* ...
*----------------------------------------------------------------------*

    es_t_entity = gs_params-et_entityset.

    es_response_context = gs_params-es_response_context_set.

    es_mo_context = gs_params-mo_context.

  ENDMETHOD.


  method ZIF_RS_3DPC_ANTWORT~GET_RESPONSE_READ.
*----------------------------------------------------------------------*
* SBB AG, SAP CCC-IT, Bern/Berne/Berna
*----------------------------------------------------------------------*
*
*-----------Änderungsdokumentation / documentation of changes----------*
* Version SAP-Rel Datum Bearbeiter
* Beschreibung Get response from entity
*----------------------------------------------------------------------*
* >000< 7.50 28.10.2020 Firma(DXC Technology) / Name(Shernan Bamba)
* Programmerstellung / creation of program
*----------------------------------------------------------------------*
* >001< 7.50 tt.mm.jjjj Firma(company) / Name(name)
* ...
*----------------------------------------------------------------------*

    es_entity = gs_params-er_entity.
    es_response_context = gs_params-es_response_context_read.
    es_mo_context = gs_params-mo_context.

  endmethod.


METHOD zif_rs_3dpc_antwort~get_response_update.
*----------------------------------------------------------------------*
* SBB AG, SAP CCC-IT, Bern/Berne/Berna
*----------------------------------------------------------------------*
*
*-----------Änderungsdokumentation / documentation of changes----------*
* Version SAP-Rel Datum Bearbeiter
* Beschreibung Get Response from update
*----------------------------------------------------------------------*
* >000< 7.50 28.10.2020 Firma(DXC Technology) / Name(Shernan Bamba)
* Programmerstellung / creation of program
*----------------------------------------------------------------------*
* >001< 7.50 tt.mm.jjjj Firma(company) / Name(name)
* ...
*----------------------------------------------------------------------*

  es_entity = gs_params-er_entity.

  es_mo_context = gs_params-mo_context.

ENDMETHOD.


  METHOD zif_rs_3dpc_entitat~post.
*----------------------------------------------------------------------*
* SBB AG, SAP CCC-IT, Bern/Berne/Berna
*----------------------------------------------------------------------*
*
*-----------Änderungsdokumentation / documentation of changes----------*
* Version SAP-Rel Datum Bearbeiter
* Beschreibung Create entityset method
*----------------------------------------------------------------------*
* >000< 7.50 28.10.2020 Firma(DXC Technology) / Name(Shernan Bamba)
* Programmerstellung / creation of program
*----------------------------------------------------------------------*
* >001< 7.50 tt.mm.jjjj Firma(company) / Name(name)
* ...
*----------------------------------------------------------------------*

    DATA: ls_project TYPE zrs_3tt_vorhaben,
          lt_return  TYPE STANDARD TABLE OF bapiret2.

    gs_params-io_data_provider->read_entry_data( IMPORTING es_data = ls_project ).

    IF ls_project IS INITIAL.

      RETURN.

    ENDIF.

    create_project(
      EXPORTING
        is_project = ls_project
      IMPORTING
        es_project = gs_params-er_entity
        et_return  = lt_return ).

    DATA(lo_response) = NEW zcl_rs_3vorhaben( is_params = gs_params ).

    rv_if_response = lo_response.

  ENDMETHOD.


METHOD zif_rs_3dpc_entitat~query.
*----------------------------------------------------------------------*
* SBB AG, SAP CCC-IT, Bern/Berne/Berna
*----------------------------------------------------------------------*
*
*-----------Änderungsdokumentation / documentation of changes----------*
* Version SAP-Rel Datum Bearbeiter
* Beschreibung Get Entityset method
*----------------------------------------------------------------------*
* >000< 7.50 28.10.2020 Firma(DXC Technology) / Name(Shernan Bamba)
* Programmerstellung / creation of program
*----------------------------------------------------------------------*
* >001< 7.50 tt.mm.jjjj Firma(company) / Name(name)
* ...
*----------------------------------------------------------------------*

  IF gs_params-iv_entity_name <> gs_params-iv_source_name.

    RETURN.

  ENDIF.

  get_data( ).

  DATA(lo_response) = NEW zcl_rs_3vorhaben( is_params = gs_params ).

  rv_if_response = lo_response.

ENDMETHOD.


  method ZIF_RS_3DPC_ENTITAT~QUERY_EXPAND.
  endmethod.


  METHOD zif_rs_3dpc_entitat~read.
*----------------------------------------------------------------------*
* SBB AG, SAP CCC-IT, Bern/Berne/Berna
*----------------------------------------------------------------------*
*
*-----------Änderungsdokumentation / documentation of changes----------*
* Version SAP-Rel Datum Bearbeiter
* Beschreibung Get Entity Methdo
*----------------------------------------------------------------------*
* >000< 7.50 28.10.2020 Firma(DXC Technology) / Name(Shernan Bamba)
* Programmerstellung / creation of program
*----------------------------------------------------------------------*
* >001< 7.50 tt.mm.jjjj Firma(company) / Name(name)
* ...
*----------------------------------------------------------------------*

    IF gs_params-iv_entity_name <> gs_params-iv_source_name.

      RETURN.

    ENDIF.

    DATA(lt_keys) = gs_params-io_tech_request_context_read->get_keys( ).

    IF NOT line_exists( lt_keys[ name = mc_key_field ] ) OR lt_keys IS INITIAL.

      RETURN.

    ENDIF.

    DATA(lv_vorhabensnummer) = lt_keys[ name = mc_key_field ]-value.

    get_project_details(
      EXPORTING
        iv_vorhabensnummer = CONV #( lv_vorhabensnummer )
      IMPORTING
        es_vorhaben        = gs_params-er_entity ).

  ENDMETHOD.


  METHOD zif_rs_3dpc_entitat~update.
*----------------------------------------------------------------------*
* SBB AG, SAP CCC-IT, Bern/Berne/Berna
*----------------------------------------------------------------------*
*
*-----------Änderungsdokumentation / documentation of changes----------*
* Version SAP-Rel Datum Bearbeiter
* Beschreibung Update entity method
*----------------------------------------------------------------------*
* >000< 7.50 28.10.2020 Firma(DXC Technology) / Name(Shernan Bamba)
* Programmerstellung / creation of program
*----------------------------------------------------------------------*
* >001< 7.50 tt.mm.jjjj Firma(company) / Name(name)
* ...
*----------------------------------------------------------------------*

    DATA: ls_project TYPE zrs_3tt_vorhaben,
          lt_return  TYPE STANDARD TABLE OF bapiret2.

    gs_params-io_data_provider->read_entry_data( IMPORTING es_data = ls_project ).

    IF ls_project IS INITIAL.

      RETURN.

    ENDIF.

    update_project(
      EXPORTING
        is_project = ls_project
      IMPORTING
        es_project = gs_params-er_entity
        et_return  = lt_return ).

    DATA(lo_response) = NEW zcl_rs_3vorhaben( is_params = gs_params ).

    rv_if_response = lo_response.

  ENDMETHOD.
ENDCLASS.
