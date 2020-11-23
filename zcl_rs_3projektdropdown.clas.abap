CLASS zcl_rs_3projektdropdown DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_rs_3dpc_antwort .
    INTERFACES zif_rs_3dpc_entitat .

    TYPES ts_dropdown TYPE zrs_3tt_dropdown_liste_s .
    TYPES:
      tt_dropdown TYPE STANDARD TABLE OF ts_dropdown WITH DEFAULT KEY .
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
        er_entity                      TYPE ts_dropdown,
        et_entityset                   TYPE tt_dropdown,
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

    CLASS-METHODS class_constructor .
    CLASS-METHODS create
      IMPORTING
        !is_params        TYPE ts_input_parameters
      RETURNING
        VALUE(ro_entitat) TYPE REF TO zif_rs_3dpc_entitat
      RAISING
        /iwbep/cx_mgw_tech_exception
        /iwbep/cx_mgw_busi_exception .
    METHODS constructor
      IMPORTING
        !is_params TYPE ts_input_parameters .
protected section.
private section.

  data GS_PARAMS type TS_INPUT_PARAMETERS .

  methods GET_DATA .
ENDCLASS.



CLASS ZCL_RS_3PROJEKTDROPDOWN IMPLEMENTATION.


  method CLASS_CONSTRUCTOR.
  endmethod.


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

    DATA(lo_projektdropdown) = NEW zcl_rs_3projektdropdown( is_params ).

    ro_entitat = lo_projektdropdown.

  endmethod.


  METHOD get_data.
*----------------------------------------------------------------------*
* SBB AG, SAP CCC-IT, Bern/Berne/Berna
*----------------------------------------------------------------------*
*
*-----------Änderungsdokumentation / documentation of changes----------*
* Version SAP-Rel Datum Bearbeiter
* Beschreibung Get data for dropdown in project screen
*----------------------------------------------------------------------*
* >000< 7.50 28.10.2020 Firma(DXC Technology) / Name(Shernan Bamba)
* Programmerstellung / creation of program
*----------------------------------------------------------------------*
* >001< 7.50 tt.mm.jjjj Firma(company) / Name(name)
* ...
*----------------------------------------------------------------------*

    DATA: lt_domain_values         TYPE STANDARD TABLE OF zrs_3tt_domainwert_s,
          lt_status_list           TYPE STANDARD TABLE OF zrs_d_3tt_prjabr,
          ls_domain_values         TYPE zrs_3tt_domainwert_s,
          ls_status_list           TYPE zrs_d_3tt_prjabr,
          ls_entityset             LIKE LINE OF gs_params-et_entityset,
          lt_filter_select_options TYPE /iwbep/t_mgw_select_option,
          lt_filter                TYPE STANDARD TABLE OF /iwbep/s_cod_select_option,
          lv_current_status        TYPE zrs_d_3tt_prjabr-estat.

    lt_filter_select_options = gs_params-io_tech_request_context_query->get_filter( )->get_filter_select_options( ).

    DATA(lo_vorhaben) = NEW zcl_rs_3vorhaben( iv_username = sy-uname ).

*"Get domain values of ZRS_3TT_DO_FUNKBERCH
    lo_vorhaben->get_domain_values(
      EXPORTING
        iv_dom_name   = 'ZRS_3TT_DO_FUNKBERCH'
        iv_langu      = sy-langu
        iv_bool       = 'X'
      IMPORTING
        et_dom_values = lt_domain_values ).

    IF lt_domain_values[] IS NOT INITIAL.

      LOOP AT lt_domain_values
      ASSIGNING FIELD-SYMBOL(<fs_dom>).

        ls_entityset-code = <fs_dom>-code.
        ls_entityset-beschreibung = <fs_dom>-beschreibung.
        ls_entityset-liste = 'FUNCTION_AREA'.

        APPEND ls_entityset TO gs_params-et_entityset.

      ENDLOOP.

    ENDIF.

*"Get domain values of ZRS_3TT_DO_TYPVERCHEL
    FREE lt_domain_values.

    lo_vorhaben->get_domain_values(
      EXPORTING
        iv_dom_name   = 'ZRS_3TT_DO_TYPVERCHEL'
        iv_langu      = sy-langu
        iv_bool       = 'X'
      IMPORTING
        et_dom_values = lt_domain_values ).

    IF lt_domain_values[] IS NOT INITIAL.

      LOOP AT lt_domain_values
      ASSIGNING <fs_dom>.

        ls_entityset-code = <fs_dom>-code.
        ls_entityset-beschreibung = <fs_dom>-beschreibung.
        ls_entityset-liste = 'ACCT_TYPE'.

        APPEND ls_entityset TO gs_params-et_entityset.

      ENDLOOP.

    ENDIF.

*"Get status based on role
    IF lt_filter_select_options[] IS NOT INITIAL.

*"Get next status
      READ TABLE lt_filter_select_options
        INTO DATA(ls_filter)
        WITH KEY property = 'CODE'.

      LOOP AT ls_filter-select_options
        INTO DATA(ls_select_options).

        MOVE: ls_select_options-low TO lv_current_status.

      ENDLOOP.

    ENDIF.

    lo_vorhaben->get_all_status(
      EXPORTING
        iv_sprache     = sy-langu
        iv_prozessart  = 'V' "Vorhaben
        iv_userid      = sy-uname
        iv_status      = lv_current_status
      IMPORTING
        et_status_list = lt_status_list ).

    IF lt_status_list[] IS NOT INITIAL.

      LOOP AT lt_status_list
        ASSIGNING FIELD-SYMBOL(<fs_stat>).

        ls_entityset-code = <fs_stat>-estat.
        ls_entityset-beschreibung = <fs_stat>-statusbeschreibung.
        ls_entityset-liste = 'STATUS'.

        APPEND ls_entityset TO gs_params-et_entityset.

      ENDLOOP.

    ENDIF.

*"Filter output
    IF lt_filter_select_options[] IS NOT INITIAL.

      READ TABLE lt_filter_select_options
        INTO ls_filter
        WITH KEY property = 'LISTE'.

      IF sy-subrc EQ 0.

        LOOP AT ls_filter-select_options
          INTO ls_select_options.

          APPEND ls_select_options TO lt_filter.

        ENDLOOP.

        DELETE gs_params-et_entityset
          WHERE liste NOT IN lt_filter.

      ENDIF.

    ENDIF.

  ENDMETHOD.


  METHOD zif_rs_3dpc_antwort~get_response_query.
*----------------------------------------------------------------------*
* SBB AG, SAP CCC-IT, Bern/Berne/Berna
*----------------------------------------------------------------------*
*
*-----------Änderungsdokumentation / documentation of changes----------*
* Version SAP-Rel Datum Bearbeiter
* Beschreibung Get response
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


  METHOD zif_rs_3dpc_entitat~query.
*----------------------------------------------------------------------*
* SBB AG, SAP CCC-IT, Bern/Berne/Berna
*----------------------------------------------------------------------*
*
*-----------Änderungsdokumentation / documentation of changes----------*
* Version SAP-Rel Datum Bearbeiter
* Beschreibung Get EntitySet method for dropdown
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

    DATA(lo_response) = NEW zcl_rs_3projektdropdown( gs_params ).

    rv_if_response = lo_response.

  ENDMETHOD.
ENDCLASS.
