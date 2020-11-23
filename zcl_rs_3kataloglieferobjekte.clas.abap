class ZCL_RS_3KATALOGLIEFEROBJEKTE definition
  public
  final
  create public

  global friends ZIF_RS_3DPC_ENTITAT .

public section.

  interfaces ZIF_RS_3DPC_ANTWORT .
  interfaces ZIF_RS_3DPC_ENTITAT .

  types TS_KATALOGLIEFEROBJEKTE type ZRS_3TT_KATLOBJ .
  types:
    tt_kataloglieferobjekte TYPE STANDARD TABLE OF ts_kataloglieferobjekte WITH DEFAULT KEY .
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
        er_entity                      TYPE ts_kataloglieferobjekte,
        et_entityset                   TYPE tt_kataloglieferobjekte,
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
  PRIVATE SECTION.

    DATA gs_params TYPE ts_input_parameters .
    CONSTANTS mc_verbinder TYPE char2 VALUE '=>' ##NO_TEXT.
    CONSTANTS:
      BEGIN OF mc_error_opt,
        business  TYPE char1 VALUE 'B',
        technical TYPE char1 VALUE 'T',
      END OF mc_error_opt .
    CONSTANTS mc_key_field TYPE string VALUE 'NUMMER_LIEFEROBJEKT' ##NO_TEXT.

    constants:
    BEGIN OF mc_filter_values,
        and                    TYPE string VALUE 'and' ##NO_TEXT,
        or                     TYPE string VALUE 'or' ##NO_TEXT,
        at_sign                TYPE string VALUE '@' ##NO_TEXT,
        ampersand              TYPE string VALUE '&' ##NO_TEXT,
        asterisk               TYPE string VALUE '*' ##NO_TEXT,
        to_read                TYPE string VALUE '( NUMMER_LIEFEROBJEKT eq `&` )' ##NO_TEXT,
      END OF mc_filter_values .

    METHODS get_data
      RAISING
        /iwbep/cx_mgw_tech_exception
        /iwbep/cx_mgw_busi_exception .
    METHODS raise_error
      IMPORTING
        !is_opt        TYPE char1 DEFAULT mc_error_opt-business
        !is_textid     LIKE if_t100_message=>t100key DEFAULT /iwbep/cx_mgw_busi_exception=>business_error
        !is_message    TYPE string OPTIONAL
        !is_methodname TYPE string OPTIONAL
      RAISING
        /iwbep/cx_mgw_tech_exception
        /iwbep/cx_mgw_busi_exception .
    METHODS get_db_values
      IMPORTING
        !is_where_clause    TYPE string DEFAULT ''
      RETURNING
        VALUE(rv_et_entity) TYPE ts_input_parameters-et_entityset
      RAISING
        /iwbep/cx_mgw_tech_exception
        /iwbep/cx_mgw_busi_exception .
ENDCLASS.



CLASS ZCL_RS_3KATALOGLIEFEROBJEKTE IMPLEMENTATION.


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
    DATA(lo_katalog) = NEW zcl_rs_3kataloglieferobjekte( is_params ).
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
    gs_params-et_entityset = get_db_values( gs_params-io_tech_request_context_query->get_osql_where_clause( ) ).

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
    DATA(lv_langu) = sy-langu.

    SELECT  mandt
            nummer_lieferobjekt
            sprache
            beschreibung
            tt_definition
     FROM zrs_3tt_katlobj
     INTO TABLE rv_et_entity
     WHERE (lv_osql_where_clause)
      AND sprache = lv_langu.

    IF rv_et_entity[] IS NOT INITIAL.
      RETURN.
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


  method ZIF_RS_3DPC_ANTWORT~GET_RESPONSE_QUERY_EXPAND.
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
  endmethod.


  METHOD zif_rs_3dpc_antwort~get_response_read.
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


  method ZIF_RS_3DPC_ANTWORT~GET_RESPONSE_READ_EXPAND.
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
  endmethod.


  METHOD zif_rs_3dpc_antwort~get_response_update.
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


  METHOD zif_rs_3dpc_entitat~delete.
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

    raise_error( is_opt = mc_error_opt-technical
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
    DATA(lo_response) = NEW zcl_rs_3kataloglieferobjekte( gs_params ).
    rv_if_response = lo_response.
  ENDMETHOD.


  method ZIF_RS_3DPC_ENTITAT~QUERY_EXPAND.
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
  endmethod.


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
      DATA(ls_entity) = lt_entity[ nummer_lieferobjekt = CONV #( lt_keys[ name = mc_key_field ]-value ) ].
      IF xsdbool( ls_entity IS NOT INITIAL ) = abap_true.
        gs_params-er_entity = ls_entity.
      ENDIF.
    ENDIF.

    DATA(lo_response) = NEW zcl_rs_3kataloglieferobjekte( gs_params ).
    rv_if_response = lo_response.
  ENDMETHOD.


  method ZIF_RS_3DPC_ENTITAT~READ_EXPAND.
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
  endmethod.


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
