CLASS zcl_rs_3benutzerollen DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_rs_3dpc_antwort .
    INTERFACES zif_rs_3dpc_entitat .

    TYPES ts_benutzerrollen TYPE zrs_3tt_benutzerrollen_s .
    TYPES:
      tt_benutzerrollen TYPE STANDARD TABLE OF ts_benutzerrollen WITH DEFAULT KEY .
    TYPES:
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
        er_entity                      TYPE ts_benutzerrollen,
        et_entityset                   TYPE tt_benutzerrollen,
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

    METHODS constructor
      IMPORTING
        !is_params TYPE ts_input_parameters
      RAISING
        /iwbep/cx_mgw_tech_exception
        /iwbep/cx_mgw_busi_exception .
    CLASS-METHODS create
      IMPORTING
        !is_params        TYPE ts_input_parameters
      RETURNING
        VALUE(ro_entitat) TYPE REF TO zif_rs_3dpc_entitat
      RAISING
        /iwbep/cx_mgw_tech_exception
        /iwbep/cx_mgw_busi_exception .
protected section.
private section.

  data GS_PARAMS type TS_INPUT_PARAMETERS .
  constants MC_VERBINDER type CHAR2 value '=>' ##NO_TEXT.
  constants:
    BEGIN OF mc_error_opt,
             business TYPE char1 VALUE 'B',
             technical TYPE char1 VALUE 'T',
             END OF mc_error_opt .
  constants MC_KEY_FIELD type STRING value 'USERID' ##NO_TEXT.

  methods RAISE_ERROR
    importing
      !IS_OPT type CHAR1
      !IS_TEXTID like IF_T100_MESSAGE=>T100KEY
      !IS_MESSAGE type STRING optional
      !IS_METHODNAME type STRING optional
    raising
      /IWBEP/CX_MGW_TECH_EXCEPTION
      /IWBEP/CX_MGW_BUSI_EXCEPTION .
  methods COPY_DATA_TO_REF
    importing
      !IS_DATA type ANY
    changing
      !CR_DATA type ref to DATA .
ENDCLASS.



CLASS ZCL_RS_3BENUTZEROLLEN IMPLEMENTATION.


  METHOD constructor.
*----------------------------------------------------------------------*
* SBB AG, SAP CCC-IT, Bern/Berne/Berna
*----------------------------------------------------------------------*
*
*-----------Änderungsdokumentation / documentation of changes----------*
* Version SAP-Rel Datum Bearbeiter
* Beschreibung Class constructor
*----------------------------------------------------------------------*
* >000< 7.50 28.10.2020 Firma(DXC Technology) / Name(Shernan Bamba)
* Programmerstellung / creation of program
*----------------------------------------------------------------------*
* >001< 7.50 tt.mm.jjjj Firma(company) / Name(name)
* ...
*----------------------------------------------------------------------*

    gs_params = is_params.

  ENDMETHOD.


  METHOD copy_data_to_ref.
*----------------------------------------------------------------------*
* SBB AG, SAP CCC-IT, Bern/Berne/Berna
*----------------------------------------------------------------------*
*
*-----------Änderungsdokumentation / documentation of changes----------*
* Version SAP-Rel Datum Bearbeiter
* Beschreibung Copy data to reference
*----------------------------------------------------------------------*
* >000< 7.50 28.10.2020 Firma(DXC Technology) / Name(Shernan Bamba)
* Programmerstellung / creation of program
*----------------------------------------------------------------------*
* >001< 7.50 tt.mm.jjjj Firma(company) / Name(name)
* ...
*----------------------------------------------------------------------*

    FIELD-SYMBOLS:
                 <ls_data> TYPE any.

    CREATE DATA cr_data LIKE is_data.
    ASSIGN cr_data->* TO <ls_data>.
    <ls_data> = is_data.

  ENDMETHOD.


  METHOD create.
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

    DATA(lo_benutzerollen) = NEW zcl_rs_3benutzerollen( is_params ).

    ro_entitat = lo_benutzerollen.

  ENDMETHOD.


  method RAISE_ERROR.
*----------------------------------------------------------------------*
* SBB AG, SAP CCC-IT, Bern/Berne/Berna
*----------------------------------------------------------------------*
*
*-----------Änderungsdokumentation / documentation of changes----------*
* Version SAP-Rel Datum Bearbeiter
* Beschreibung Raise error method
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
  endmethod.


  method ZIF_RS_3DPC_ANTWORT~GET_RESPONSE_QUERY.
  endmethod.


  METHOD zif_rs_3dpc_antwort~get_response_query_expand.
*----------------------------------------------------------------------*
* SBB AG, SAP CCC-IT, Bern/Berne/Berna
*----------------------------------------------------------------------*
*
*-----------Änderungsdokumentation / documentation of changes----------*
* Version SAP-Rel Datum Bearbeiter
* Beschreibung Get Response query from expand
*----------------------------------------------------------------------*
* >000< 7.50 28.10.2020 Firma(DXC Technology) / Name(Shernan Bamba)
* Programmerstellung / creation of program
*----------------------------------------------------------------------*
* >001< 7.50 tt.mm.jjjj Firma(company) / Name(name)
* ...
*----------------------------------------------------------------------*

    es_entityset = gs_params-er_entityset_expand.
    es_t_expanded_clauses = gs_params-et_expanded_clauses.
    es_t_expanded_tech_clauses = gs_params-et_expanded_tech_clauses .
    es_response_context = gs_params-es_response_context_set.

  ENDMETHOD.


  METHOD zif_rs_3dpc_antwort~get_response_read.
*----------------------------------------------------------------------*
* SBB AG, SAP CCC-IT, Bern/Berne/Berna
*----------------------------------------------------------------------*
*
*-----------Änderungsdokumentation / documentation of changes----------*
* Version SAP-Rel Datum Bearbeiter
* Beschreibung Get Response
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

  ENDMETHOD.


  METHOD zif_rs_3dpc_antwort~get_response_read_expand.
*----------------------------------------------------------------------*
* SBB AG, SAP CCC-IT, Bern/Berne/Berna
*----------------------------------------------------------------------*
*
*-----------Änderungsdokumentation / documentation of changes----------*
* Version SAP-Rel Datum Bearbeiter
* Beschreibung Get Response read from expand
*----------------------------------------------------------------------*
* >000< 7.50 28.10.2020 Firma(DXC Technology) / Name(Shernan Bamba)
* Programmerstellung / creation of program
*----------------------------------------------------------------------*
* >001< 7.50 tt.mm.jjjj Firma(company) / Name(name)
* ...
*----------------------------------------------------------------------*

    es_entity = gs_params-er_entity_expand.
    es_t_expanded_clauses = gs_params-et_expanded_clauses.
    es_t_expanded_tech_clauses = gs_params-et_expanded_tech_clauses .
    es_response_context = gs_params-es_response_context_read.

  ENDMETHOD.


  method ZIF_RS_3DPC_ENTITAT~QUERY.
  endmethod.


  METHOD zif_rs_3dpc_entitat~query_expand.
*----------------------------------------------------------------------*
* SBB AG, SAP CCC-IT, Bern/Berne/Berna
*----------------------------------------------------------------------*
*
*-----------Änderungsdokumentation / documentation of changes----------*
* Version SAP-Rel Datum Bearbeiter
* Beschreibung Expand EntitySet For Roles
*----------------------------------------------------------------------*
* >000< 7.50 28.10.2020 Firma(DXC Technology) / Name(Shernan Bamba)
* Programmerstellung / creation of program
*----------------------------------------------------------------------*
* >001< 7.50 tt.mm.jjjj Firma(company) / Name(name)
* ...
*----------------------------------------------------------------------*

    TYPES: ty_s_user TYPE zrs_3tt_benutzerrollen_s,
           BEGIN OF ty_s_roles,
             role         TYPE string,
             description  TYPE string,
             ist_assigned TYPE boolean,
           END OF ty_s_roles,
           ty_t_roles TYPE STANDARD TABLE OF ty_s_roles WITH DEFAULT KEY.

    DATA: BEGIN OF ls_userdetails.
            INCLUDE TYPE ty_s_user.
            DATA: rolleset TYPE ty_t_roles,
          END OF ls_userdetails.
    TYPES: tt_user LIKE STANDARD TABLE OF ls_userdetails WITH DEFAULT KEY.
    DATA: lt_user TYPE tt_user,
          ls_user LIKE LINE OF lt_user.

    DATA(lv_userid) = sy-uname.

    DATA(lo_vorhaben) = NEW zcl_rs_3vorhaben( iv_username = lv_userid ).

    ls_user = CORRESPONDING #( lo_vorhaben->gs_role ).

    ls_user-rolleset = VALUE #(
    (
      role = 'SMSBB'

      ist_assigned = SWITCH #( lo_vorhaben->gs_role-ist_sbb_service_management
                               WHEN 'X' THEN abap_true
                               ELSE abap_false
                              )
    )
    (
      role = 'DSPOC'

      ist_assigned = SWITCH #( lo_vorhaben->gs_role-ist_dev_spoc
                               WHEN 'X' THEN abap_true
                               ELSE abap_false
                              )
    )
    (
      role = 'PLSWA'

      ist_assigned = SWITCH #( lo_vorhaben->gs_role-ist_projektleiter_swa
                               WHEN 'X' THEN abap_true
                               ELSE abap_false
                              )
    )
    (
      role = 'DXRUN'

      ist_assigned = SWITCH #( lo_vorhaben->gs_role-ist_dxc_run
                               WHEN 'X' THEN abap_true
                               ELSE abap_false
                              )
    )
    ).

    APPEND ls_user TO lt_user.

    copy_data_to_ref(
          EXPORTING
           is_data = lt_user
          CHANGING
           cr_data = gs_params-er_entityset_expand ).

    DATA(lo_response) = NEW zcl_rs_3benutzerollen( gs_params ).

    rv_if_response = lo_response.

  ENDMETHOD.


  METHOD zif_rs_3dpc_entitat~read.
*----------------------------------------------------------------------*
* SBB AG, SAP CCC-IT, Bern/Berne/Berna
*----------------------------------------------------------------------*
*
*-----------Änderungsdokumentation / documentation of changes----------*
* Version SAP-Rel Datum Bearbeiter
* Beschreibung Get Entity Method
*----------------------------------------------------------------------*
* >000< 7.50 28.10.2020 Firma(DXC Technology) / Name(Shernan Bamba)
* Programmerstellung / creation of program
*----------------------------------------------------------------------*
* >001< 7.50 tt.mm.jjjj Firma(company) / Name(name)
* ...
*----------------------------------------------------------------------*

    DATA(lt_keys) = gs_params-io_tech_request_context_read->get_keys( ).

    IF line_exists( lt_keys[ name = mc_key_field ] ) OR lt_keys IS NOT INITIAL.

      DATA(ls_keys) = lt_keys[ name = mc_key_field ].

    ENDIF.

    IF ls_keys IS NOT INITIAL.

      IF ls_keys-value EQ 'X'.

        DATA(lv_userid) = sy-uname.

      ELSE.

        lv_userid = ls_keys-value.

      ENDIF.

    ENDIF.

    DATA(lo_vorhaben) = NEW zcl_rs_3vorhaben( iv_username = lv_userid ).

    gs_params-er_entity = lo_vorhaben->gs_role.

    DATA(lo_response) = NEW zcl_rs_3benutzerollen( gs_params ).

    rv_if_response = lo_response.

  ENDMETHOD.


  METHOD zif_rs_3dpc_entitat~read_expand.
*----------------------------------------------------------------------*
* SBB AG, SAP CCC-IT, Bern/Berne/Berna
*----------------------------------------------------------------------*
*
*-----------Änderungsdokumentation / documentation of changes----------*
* Version SAP-Rel Datum Bearbeiter
* Beschreibung Expand Entity For Roles
*----------------------------------------------------------------------*
* >000< 7.50 28.10.2020 Firma(DXC Technology) / Name(Shernan Bamba)
* Programmerstellung / creation of program
*----------------------------------------------------------------------*
* >001< 7.50 tt.mm.jjjj Firma(company) / Name(name)
* ...
*----------------------------------------------------------------------*

    TYPES: ty_s_user TYPE zrs_3tt_benutzerrollen_s,
           BEGIN OF ty_s_roles,
             role         TYPE string,
             description  TYPE string,
             ist_assigned TYPE boolean,
           END OF ty_s_roles,
           ty_t_roles TYPE STANDARD TABLE OF ty_s_roles WITH DEFAULT KEY.

    DATA: BEGIN OF ls_userdetails.
            INCLUDE TYPE ty_s_user.
            DATA: rolleset TYPE ty_t_roles,
          END OF ls_userdetails.
    TYPES: tt_user LIKE STANDARD TABLE OF ls_userdetails WITH DEFAULT KEY.
    DATA: lt_user TYPE tt_user,
          ls_user LIKE LINE OF lt_user.

    DATA(lt_keys) = gs_params-io_tech_request_context_read->get_keys( ).

    IF line_exists( lt_keys[ name = mc_key_field ] ) OR lt_keys IS NOT INITIAL.

      DATA(ls_keys) = lt_keys[ name = mc_key_field ].

    ENDIF.

    IF ls_keys IS NOT INITIAL.

      IF ls_keys-value EQ 'X'.

        DATA(lv_userid) = sy-uname.

      ELSE.

        lv_userid = ls_keys-value.

      ENDIF.

    ENDIF.

    DATA(lo_vorhaben) = NEW zcl_rs_3vorhaben( iv_username = lv_userid ).

    ls_user = CORRESPONDING #( lo_vorhaben->gs_role ).

    ls_user-rolleset = VALUE #(
    (
      role = 'SMSBB'

      ist_assigned = SWITCH #( lo_vorhaben->gs_role-ist_sbb_service_management
                               WHEN 'X' THEN abap_true
                               ELSE abap_false
                              )
    )
    (
      role = 'DSPOC'

      ist_assigned = SWITCH #( lo_vorhaben->gs_role-ist_dev_spoc
                               WHEN 'X' THEN abap_true
                               ELSE abap_false
                              )
    )
    (
      role = 'PLSWA'

      ist_assigned = SWITCH #( lo_vorhaben->gs_role-ist_projektleiter_swa
                               WHEN 'X' THEN abap_true
                               ELSE abap_false
                              )
    )
    (
      role = 'DXRUN'

      ist_assigned = SWITCH #( lo_vorhaben->gs_role-ist_dxc_run
                               WHEN 'X' THEN abap_true
                               ELSE abap_false
                              )
    )
    ).

    copy_data_to_ref(
          EXPORTING
           is_data = ls_user
          CHANGING
           cr_data = gs_params-er_entity_expand ).

    DATA(lo_response) = NEW zcl_rs_3benutzerollen( gs_params ).

    rv_if_response = lo_response.


  ENDMETHOD.
ENDCLASS.
