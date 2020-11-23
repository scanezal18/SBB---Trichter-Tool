class ZCL_RS_3VERRCHNG definition
  public
  final
  create public

  global friends ZIF_RS_3DPC_ABRUFSTATUS
                 ZIF_RS_3DPC_ANTWORT
                 ZIF_RS_3DPC_ENTITAT .

public section.

  interfaces ZIF_RS_3DPC_ANTWORT .
  interfaces ZIF_RS_3DPC_ENTITAT .

  types TS_VERRCHNG type ZRS_3TT_VERRCHNG_S .
  types:
    tt_verrchng TYPE STANDARD TABLE OF ts_verrchng WITH DEFAULT KEY .
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
        er_entity                      TYPE ts_verrchng,
        et_entityset                   TYPE tt_verrchng,
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

    TYPES ts_s_verrchng TYPE zrs_3tt_verrchng .
    TYPES:
      tt_t_verrchng TYPE STANDARD TABLE OF ts_s_verrchng WITH DEFAULT KEY .

    DATA gs_params TYPE ts_input_parameters .
    CONSTANTS mc_verbinder TYPE char2 VALUE '=>' ##NO_TEXT.
    CONSTANTS:
      BEGIN OF mc_filter_values,
        status_filter          TYPE string VALUE '( Status eq `&` )' ##NO_TEXT,
        sprache_filter         TYPE string VALUE '( Sprache eq `&` )' ##NO_TEXT,
        vorhabensnummer_filter TYPE string VALUE '( Vorhabensnummer eq `&` )' ##NO_TEXT,
        and                    TYPE string VALUE 'and' ##NO_TEXT,
        or                     TYPE string VALUE 'or' ##NO_TEXT,
        at_sign                TYPE string VALUE '@' ##NO_TEXT,
        ampersand              TYPE string VALUE '&' ##NO_TEXT,
        asterisk               TYPE string VALUE '*' ##NO_TEXT,
        to_read_vorhaben       TYPE string VALUE '( Vorhabensnummer eq `&` and Status eq `@` )' ##NO_TEXT,
        to_read                TYPE string VALUE '( Abrufnummer eq `&` and Abrufposition eq `@` and Lieferanteil eq `*` )' ##NO_TEXT,
      END OF mc_filter_values .
    CONSTANTS:
      BEGIN OF mc_error_opt,
        business  TYPE char1 VALUE 'B',
        technical TYPE char1 VALUE 'T',
      END OF mc_error_opt .
    CONSTANTS:
      BEGIN OF mc_cud_type,
        create TYPE char1 VALUE 'C',
        update TYPE char1 VALUE 'U',
        delete TYPE char1 VALUE 'D',
      END OF mc_cud_type .
    CONSTANTS: BEGIN OF mc_key_fields,
                 abrufnummer   TYPE string VALUE 'ABRUFNUMMER' ##NO_TEXT,
                 abrufposition TYPE string VALUE 'ABRUFPOSITION' ##NO_TEXT,
                 lieferanteil  TYPE string VALUE 'LIEFERANTEIL' ##NO_TEXT,
               END OF mc_key_fields.

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
    METHODS generate_abruf_nummer
      IMPORTING
        !is_nr_range     TYPE inri-nrrangenr DEFAULT zif_rs_3constants=>gc_abruf_nr_range
        !is_object       TYPE inri-object DEFAULT zif_rs_3constants=>gc_abruf_nr_object
      RETURNING
        VALUE(rv_number) TYPE string
      RAISING
        /iwbep/cx_mgw_tech_exception
        /iwbep/cx_mgw_busi_exception .
    METHODS submit_db_values
      IMPORTING
        !is_t_verrchng TYPE tt_t_verrchng
      RAISING
        /iwbep/cx_mgw_tech_exception
        /iwbep/cx_mgw_busi_exception .
    METHODS copy_data_to_ref
      IMPORTING
        !is_data TYPE any
      CHANGING
        !cs_data TYPE REF TO data
      RAISING
        /iwbep/cx_mgw_tech_exception
        /iwbep/cx_mgw_busi_exception .
    METHODS map_entity_to_db_struc
      IMPORTING
        !is_er_entity       TYPE ts_input_parameters-er_entity
      RETURNING
        VALUE(rv_er_entity) TYPE ts_s_verrchng
      RAISING
        /iwbep/cx_mgw_tech_exception
        /iwbep/cx_mgw_busi_exception .
ENDCLASS.



CLASS ZCL_RS_3VERRCHNG IMPLEMENTATION.


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


  METHOD copy_data_to_ref.
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
    FIELD-SYMBOLS <ls_data> TYPE any.
    CREATE DATA cs_data LIKE is_data.
    ASSIGN cs_data->* TO <ls_data>.
    <ls_data> = is_data.
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
    DATA(lo_abruf) = NEW zcl_rs_3verrchng( is_params ).
    ro_entitat = lo_abruf.
  ENDMETHOD.


  METHOD generate_abruf_nummer.
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
    DATA lv_message TYPE string.
    CALL FUNCTION 'NUMBER_GET_NEXT'
      EXPORTING
        nr_range_nr             = is_nr_range
        object                  = is_object
      IMPORTING
        number                  = rv_number
      EXCEPTIONS
        interval_not_found      = 1
        number_range_not_intern = 2
        object_not_found        = 3
        quantity_is_0           = 4
        quantity_is_not_1       = 5
        interval_overflow       = 6
        buffer_overflow         = 7
        OTHERS                  = 8.
    IF sy-subrc = 0.
*       Implement suitable error handling here
      COMMIT WORK.
    ENDIF.

    CONDENSE rv_number.
    IF rv_number IS INITIAL.
      MESSAGE i036(zrs_3) INTO lv_message.
      raise_error( is_message = lv_message ).
    ENDIF.
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

    DATA(lt_entity) = get_db_values( lv_osql_where_clause ).

    gs_params-et_entityset = lt_entity.

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

    SELECT  mandt
            abrufnummer
            abrufposition
            lieferanteil
            lieferdatum
            abnahmedatum
            kz_nichtabgenommen
            kz_verrechnet
            verrechnungsdatum
            wert_position
            kosten_position
            einheit_wert
            waehrung
            kosten_anteil
            wert_pos_anteil
    FROM zrs_d_3tt_verrch
    INTO TABLE rv_et_entity
    WHERE (lv_osql_where_clause).
    IF rv_et_entity IS NOT INITIAL.
      RETURN.
    ENDIF.
  ENDMETHOD.


  METHOD map_entity_to_db_struc.
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
    DATA(ls_entity) = is_er_entity.
    CLEAR rv_er_entity.
    IF ls_entity IS INITIAL.
       RETURN.
    ENDIF.
    rv_er_entity = VALUE ts_s_verrchng( mandt	= ls_entity-mandt
                                        abrufnummer	= ls_entity-abrufnummer
                                        abrufposition	= ls_entity-abrufposition
                                        lieferanteil  = ls_entity-lieferanteil
                                        lieferdatum	= ls_entity-lieferdatum
                                        abnahmedatum  = ls_entity-abnahmedatum
                                        kz_nichtabgenommen  = ls_entity-kz_nichtabgenommen
                                        kz_verrechnet	= ls_entity-kz_verrechnet
                                        verrechnungsdatum	= ls_entity-verrechnungsdatum
                                        wert_position	= ls_entity-wert_position
                                        kosten_position	= ls_entity-kosten_position
                                        einheit_wert  = ls_entity-einheit_wert
                                        waehrung  = ls_entity-waehrung
                                        kosten_anteil	= ls_entity-kosten_anteil
                                        wert_pos_anteil	= ls_entity-wert_pos_anteil ) .

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


  METHOD submit_db_values.
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
    DATA(lt_verrchng) = is_t_verrchng.
    DATA lv_message TYPE string.

    IF lines( lt_verrchng ) >= 1.
      "insert value/update data
      MODIFY zrs_3tt_verrchng FROM TABLE lt_verrchng.
      CASE sy-subrc.
        WHEN 0.
          COMMIT WORK.
        WHEN OTHERS.
          MESSAGE i035(zrs_3) INTO lv_message.
          raise_error( is_message = lv_message ).
      ENDCASE.
    ENDIF.
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
    es_entity = gs_params-er_entity.
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
    es_r_deep_entity = gs_params-er_deep_entity.
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
    es_entityset = gs_params-er_entityset_expand.
    es_t_expanded_clauses = gs_params-et_expanded_clauses.
    es_t_expanded_tech_clauses = gs_params-et_expanded_tech_clauses .
    es_response_context = gs_params-es_response_context_set.
  ENDMETHOD.


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
    es_entity = gs_params-er_entity_expand.
    es_response_context = gs_params-es_response_context_read.
    es_t_expanded_clauses = gs_params-et_expanded_clauses.
    es_t_expanded_tech_clauses = gs_params-et_expanded_tech_clauses.
  ENDMETHOD.


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
    es_entity = gs_params-er_entity.
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
    DATA: ls_entity  LIKE gs_params-er_entity,
          lv_message TYPE string,
          lt_entity  TYPE tt_t_verrchng.
    gs_params-io_data_provider->read_entry_data( IMPORTING es_data = ls_entity ).
    IF ls_entity IS INITIAL.
      RETURN.
    ENDIF.
    DATA(lo_response) = NEW zcl_rs_3verrchng( gs_params ).
    rv_if_response = lo_response.
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
    DATA(lt_stacks) = cl_abap_get_call_stack=>format_call_stack_with_struct( cl_abap_get_call_stack=>get_call_stack( ) ).
    IF lt_stacks IS NOT INITIAL.
      DATA(lv_method_name) = substring_after( val = lt_stacks[ 1 ]-event sub = mc_verbinder ).
    ENDIF.

    raise_error( is_opt = mc_error_opt-technical
                    is_textid = /iwbep/cx_mgw_not_impl_exc=>method_not_implemented
                    is_methodname = lv_method_name ).
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
    IF gs_params-iv_entity_name <> gs_params-iv_source_name.
      RETURN.
    ENDIF.

    get_data( ).
    DATA(lo_response) = NEW zcl_rs_3verrchng( gs_params ).
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
    IF gs_params-iv_entity_name <> gs_params-iv_source_name.
      RETURN.
    ENDIF.

    DATA(lo_response) = NEW zcl_rs_3verrchng( gs_params ).
    rv_if_response = lo_response.
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

    DATA(lv_osql_where_clause) = mc_filter_values-to_read.
    DATA(lt_keys) = gs_params-io_tech_request_context_read->get_keys( ).
    IF NOT line_exists( lt_keys[ name = mc_key_fields-abrufnummer ] ) OR lt_keys IS INITIAL.
      RETURN.
    ENDIF.

    DATA(lv_abrufnummer) = lt_keys[ name = mc_key_fields-abrufnummer ]-value.
    REPLACE ALL OCCURRENCES: OF mc_filter_values-ampersand IN lv_osql_where_clause WITH lv_abrufnummer.
    CONDENSE lv_osql_where_clause.

    IF NOT line_exists( lt_keys[ name = mc_key_fields-abrufposition ] ) OR lt_keys IS INITIAL.
      RETURN.
    ENDIF.

    DATA(lv_abrufposition) = lt_keys[ name = mc_key_fields-abrufposition ]-value.
    REPLACE ALL OCCURRENCES: OF mc_filter_values-at_sign IN lv_osql_where_clause WITH lv_abrufposition.
    CONDENSE lv_osql_where_clause.

    IF NOT line_exists( lt_keys[ name = mc_key_fields-lieferanteil ] ) OR lt_keys IS INITIAL.
      RETURN.
    ENDIF.

    DATA(lv_lieferanteil) = lt_keys[ name = mc_key_fields-lieferanteil ]-value.
    REPLACE ALL OCCURRENCES: OF mc_filter_values-asterisk IN lv_osql_where_clause WITH lv_lieferanteil.
    CONDENSE lv_osql_where_clause.

    DATA(lt_entity) = get_db_values( lv_osql_where_clause ).

    IF lt_entity IS NOT INITIAL.
      gs_params-er_entity = lt_entity[ 1 ].
    ENDIF.

    DATA(lo_response) = NEW zcl_rs_3verrchng( gs_params ).
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
    DATA(lt_stacks) = cl_abap_get_call_stack=>format_call_stack_with_struct( cl_abap_get_call_stack=>get_call_stack( ) ).
    IF lt_stacks IS NOT INITIAL.
      DATA(lv_method_name) = substring_after( val = lt_stacks[ 1 ]-event sub = mc_verbinder ).
    ENDIF.

    raise_error( is_opt = mc_error_opt-technical
                    is_textid = /iwbep/cx_mgw_not_impl_exc=>method_not_implemented
                    is_methodname = lv_method_name ).
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
    DATA: ls_entity LIKE gs_params-er_entity,
          lt_entity TYPE tt_verrchng.
    gs_params-io_data_provider->read_entry_data( IMPORTING es_data = ls_entity ).
    gs_params-er_entity = ls_entity.
    IF ls_entity IS INITIAL.
      RETURN.
    ENDIF.

    DATA(lo_response) = NEW zcl_rs_3verrchng( gs_params ).
    rv_if_response = lo_response.
  ENDMETHOD.
ENDCLASS.
