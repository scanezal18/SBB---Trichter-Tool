class ZCL_RS_3DOMAIN_EINHEIT_WERT definition
  public
  final
  create public .

public section.

  interfaces ZIF_RS_3DPC_ANTWORT .
  interfaces ZIF_RS_3DPC_ENTITAT .

  types TS_DOMAINWERT type ZRS_3TT_DOMAINWERT_S .
  types:
    tt_domainwert TYPE STANDARD TABLE OF ts_domainwert WITH DEFAULT KEY .
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
        er_entity                      TYPE ts_domainwert,
        et_entityset                   TYPE tt_domainwert,
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

  class-methods CREATE
    importing
      !IS_PARAMS type TS_INPUT_PARAMETERS
    returning
      value(RO_ENTITAT) type ref to ZIF_RS_3DPC_ENTITAT
    raising
      /IWBEP/CX_MGW_TECH_EXCEPTION
      /IWBEP/CX_MGW_BUSI_EXCEPTION .
  methods CONSTRUCTOR
    importing
      !IS_PARAMS type TS_INPUT_PARAMETERS
    raising
      /IWBEP/CX_MGW_TECH_EXCEPTION
      /IWBEP/CX_MGW_BUSI_EXCEPTION .
  PROTECTED SECTION.
private section.
  CONSTANTS: BEGIN OF mc_filter_params,
                default TYPE string  VALUE `( MSEHI eq 'AW' OR MSEHI eq 'TP' )`,
                code_field TYPE string VALUE 'Code',
                msehi_field TYPE string VALUE 'MSEHI',
             END OF mc_filter_params.
  types:
    BEGIN OF ts_t006,
             msehi TYPE t006-msehi,
           END OF ts_t006 .
  types:
    tt_t006 TYPE STANDARD TABLE OF ts_t006 WITH DEFAULT KEY .

  data gs_params type TS_INPUT_PARAMETERS .
  constants MC_VERBINDER type CHAR2 value '=>' ##NO_TEXT.
  constants:
    BEGIN OF mc_error_opt,
        business  TYPE char1 VALUE 'B',
        technical TYPE char1 VALUE 'T',
      END OF mc_error_opt,
      mc_key_field TYPE string VALUE 'CODE'.

  methods GET_DATA
    raising
      /IWBEP/CX_MGW_TECH_EXCEPTION
      /IWBEP/CX_MGW_BUSI_EXCEPTION .
  methods RAISE_ERROR
    importing
      !IS_OPT type CHAR1 default MC_ERROR_OPT-BUSINESS
      !IS_TEXTID like IF_T100_MESSAGE=>T100KEY default /IWBEP/CX_MGW_BUSI_EXCEPTION=>BUSINESS_ERROR
      !IS_MESSAGE type string optional
      !IS_METHODNAME type STRING optional
    raising
      /IWBEP/CX_MGW_TECH_EXCEPTION
      /IWBEP/CX_MGW_BUSI_EXCEPTION .
  methods GET_DB_VALUES
    importing
      !IS_WHERE_CLAUSE type STRING default ''
    returning
      value(RV_ET_ENTITY) type TS_INPUT_PARAMETERS-ET_ENTITYSET
    raising
      /IWBEP/CX_MGW_TECH_EXCEPTION
      /IWBEP/CX_MGW_BUSI_EXCEPTION .
ENDCLASS.



CLASS ZCL_RS_3DOMAIN_EINHEIT_WERT IMPLEMENTATION.


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
    DATA(lo_katalog) = NEW zcl_rs_3domain_einheit_wert( is_params ).
    ro_entitat = lo_katalog.
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
    DATA(lv_osql_where_clause) = gs_params-io_tech_request_context_query->get_osql_where_clause( ).
    CONDENSE lv_osql_where_clause.

    IF lv_osql_where_clause IS INITIAL.
      lv_osql_where_clause = mc_filter_params-default.
    ELSE.
      REPLACE ALL OCCURRENCES OF mc_filter_params-code_field IN lv_osql_where_clause WITH mc_filter_params-msehi_field.
    ENDIF.

    gs_params-et_entityset = get_db_values( lv_osql_where_clause ).

    IF gs_params-et_entityset[] IS NOT INITIAL.
      RETURN.
    ENDIF.
  ENDMETHOD.


  METHOD get_db_values.
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
    DATA(lv_osql_where_clause) = is_where_clause.
    CONDENSE lv_osql_where_clause.
    DATA lt_t006 TYPE tt_t006.

    SELECT msehi
              INTO TABLE lt_t006
              FROM t006
              WHERE (lv_osql_where_clause).

    IF lt_t006 IS INITIAL.
      RETURN.
    ENDIF.

    rv_et_entity = VALUE #( FOR lw_t006 IN lt_t006
                                  ( code = lw_t006-msehi
                                    beschreibung = lw_t006-msehi )
                                    ).

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


  METHOD zif_rs_3dpc_antwort~get_response_delete.
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
    DATA(lt_stacks) = cl_abap_get_call_stack=>format_call_stack_with_struct( cl_abap_get_call_stack=>get_call_stack( ) ).
    IF lt_stacks IS NOT INITIAL.
      DATA(lv_method_name) = substring_after( val = lt_stacks[ 1 ]-event sub = mc_verbinder ).
    ENDIF.

    raise_error( is_opt = mc_error_opt-technical
                    is_textid = /iwbep/cx_mgw_not_impl_exc=>method_not_implemented
                    is_methodname = lv_method_name ).
  ENDMETHOD.


  METHOD zif_rs_3dpc_antwort~get_response_post.
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
    DATA(lt_stacks) = cl_abap_get_call_stack=>format_call_stack_with_struct( cl_abap_get_call_stack=>get_call_stack( ) ).
    IF lt_stacks IS NOT INITIAL.
      DATA(lv_method_name) = substring_after( val = lt_stacks[ 1 ]-event sub = mc_verbinder ).
    ENDIF.

    raise_error( is_opt = mc_error_opt-technical
                    is_textid = /iwbep/cx_mgw_not_impl_exc=>method_not_implemented
                    is_methodname = lv_method_name ).
  ENDMETHOD.


  METHOD zif_rs_3dpc_antwort~get_response_post_deep_entity.
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
  ENDMETHOD.


  METHOD zif_rs_3dpc_antwort~get_response_query.
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
    es_t_entity = gs_params-et_entityset.
    es_response_context = gs_params-es_response_context_set.
    es_mo_context = gs_params-mo_context.
  ENDMETHOD.


  METHOD zif_rs_3dpc_antwort~get_response_query_expand.
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
  ENDMETHOD.


  METHOD ZIF_RS_3DPC_ANTWORT~GET_RESPONSE_READ.
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
    es_entity = gs_params-er_entity.
    es_response_context = gs_params-es_response_context_read.
    es_mo_context = gs_params-mo_context.
  ENDMETHOD.


  METHOD zif_rs_3dpc_antwort~get_response_read_expand.
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
  ENDMETHOD.


  METHOD ZIF_RS_3DPC_ANTWORT~GET_RESPONSE_UPDATE.
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
    DATA(lt_stacks) = cl_abap_get_call_stack=>format_call_stack_with_struct( cl_abap_get_call_stack=>get_call_stack( ) ).
    IF lt_stacks IS NOT INITIAL.
      DATA(lv_method_name) = substring_after( val = lt_stacks[ 1 ]-event sub = mc_verbinder ).
    ENDIF.

    RAISE_ERROR( is_opt = mc_error_opt-technical
                    is_textid = /iwbep/cx_mgw_not_impl_exc=>method_not_implemented
                    is_methodname = lv_method_name ).
  ENDMETHOD.


  METHOD ZIF_RS_3DPC_ENTITAT~DELETE.
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
    DATA(lt_stacks) = cl_abap_get_call_stack=>format_call_stack_with_struct( cl_abap_get_call_stack=>get_call_stack( ) ).
    IF lt_stacks IS NOT INITIAL.
      DATA(lv_method_name) = substring_after( val = lt_stacks[ 1 ]-event sub = mc_verbinder ).
    ENDIF.

    RAISE_ERROR( is_opt = mc_error_opt-technical
                    is_textid = /iwbep/cx_mgw_not_impl_exc=>method_not_implemented
                    is_methodname = lv_method_name ).
  ENDMETHOD.


  METHOD zif_rs_3dpc_entitat~post.
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
    DATA(lt_stacks) = cl_abap_get_call_stack=>format_call_stack_with_struct( cl_abap_get_call_stack=>get_call_stack( ) ).
    IF lt_stacks IS NOT INITIAL.
      DATA(lv_method_name) = substring_after( val = lt_stacks[ 1 ]-event sub = mc_verbinder ).
    ENDIF.

    RAISE_ERROR( is_opt = mc_error_opt-technical
                    is_textid = /iwbep/cx_mgw_not_impl_exc=>method_not_implemented
                    is_methodname = lv_method_name ).
  ENDMETHOD.


  METHOD zif_rs_3dpc_entitat~post_deep_entity.
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
  ENDMETHOD.


  METHOD zif_rs_3dpc_entitat~query.
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
    get_data( ).
    DATA(lo_response) = NEW zcl_rs_3domain_einheit_wert( gs_params ).
    rv_if_response = lo_response.
  ENDMETHOD.


  METHOD zif_rs_3dpc_entitat~query_expand.
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
  ENDMETHOD.


  METHOD zif_rs_3dpc_entitat~read.
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
    IF gs_params-iv_entity_name <> gs_params-iv_source_name.
      RETURN.
    ENDIF.

    DATA(lt_keys) = gs_params-io_tech_request_context_read->get_keys( ).
    DATA(lt_entity) = get_db_values( is_where_clause = space ).
    IF line_exists( lt_keys[ name = mc_key_field ] ) AND
       lt_entity IS NOT INITIAL.
      DATA(ls_entity) = lt_entity[ code = CONV #( lt_keys[ name = mc_key_field ]-value ) ].
      IF xsdbool( ls_entity IS NOT INITIAL ) = abap_true.
        gs_params-er_entity = ls_entity.
      ENDIF.
    ENDIF.

    DATA(lo_response) = NEW zcl_rs_3domain_einheit_wert( gs_params ).
    rv_if_response = lo_response.
  ENDMETHOD.


  METHOD zif_rs_3dpc_entitat~read_expand.
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
  ENDMETHOD.


  METHOD zif_rs_3dpc_entitat~update.
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
    DATA(lt_stacks) = cl_abap_get_call_stack=>format_call_stack_with_struct( cl_abap_get_call_stack=>get_call_stack( ) ).
    IF lt_stacks IS NOT INITIAL.
      DATA(lv_method_name) = substring_after( val = lt_stacks[ 1 ]-event sub = mc_verbinder ).
    ENDIF.

    raise_error( is_opt = mc_error_opt-technical
                    is_textid = /iwbep/cx_mgw_not_impl_exc=>method_not_implemented
                    is_methodname = lv_method_name ).
  ENDMETHOD.
ENDCLASS.
