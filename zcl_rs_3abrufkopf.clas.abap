CLASS zcl_rs_3abrufkopf DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC

  GLOBAL FRIENDS zif_rs_3dpc_abrufstatus
                 zif_rs_3dpc_entitat .

  PUBLIC SECTION.

    INTERFACES zif_rs_3dpc_antwort .
    INTERFACES zif_rs_3dpc_entitat .

    TYPES ts_abrufkopf TYPE zrs_3tt_abrufkopf_s .
    TYPES:
      tt_abrufkopf TYPE STANDARD TABLE OF ts_abrufkopf WITH DEFAULT KEY .
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
        er_entity                      TYPE ts_abrufkopf,
        et_entityset                   TYPE tt_abrufkopf,
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

    DATA:
      BEGIN OF gs_deep_abrufkopf.
        INCLUDE TYPE ts_abrufkopf.
        DATA: tostatusset TYPE zcl_rs_3prjabrstat=>tt_prjabrst,
      END OF gs_deep_abrufkopf .

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
        !is_params TYPE ts_input_parameters
      RAISING
        /iwbep/cx_mgw_tech_exception
        /iwbep/cx_mgw_busi_exception .
  PROTECTED SECTION.
private section.

  types TS_S_ABRUFKOPF type ZRS_3TT_ABRUFKOP .
  types:
    tt_t_abrufkopf TYPE STANDARD TABLE OF ts_s_abrufkopf WITH DEFAULT KEY .
  types:
    tt_deep_abrufkopf LIKE STANDARD TABLE OF gs_deep_abrufkopf WITH DEFAULT KEY .

  data GS_PARAMS type TS_INPUT_PARAMETERS .
  constants MC_VERBINDER type CHAR2 value '=>' ##NO_TEXT.
  constants:
    BEGIN OF mc_filter_values,
        status_filter          TYPE string VALUE '( Status eq `&` )' ##NO_TEXT,
        prjabrstat_filter      TYPE string VALUE '( Prozessart eq `@` and Status eq `&` )' ##NO_TEXT,
        mandt_filter           TYPE string VALUE '( Mandt eq `&` )' ##NO_TEXT,
        sprache_filter         TYPE string VALUE '( Sprache eq `&` )' ##NO_TEXT,
        abrufnummer_filter     TYPE string VALUE '( Abrufnummer eq `&` )' ##NO_TEXT,
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
  constants:
    BEGIN OF mc_error_opt,
        business  TYPE char1 VALUE 'B',
        technical TYPE char1 VALUE 'T',
      END OF mc_error_opt .
  constants:
    BEGIN OF mc_cud_type,
        create TYPE char1 VALUE 'C',
        update TYPE char1 VALUE 'U',
        delete TYPE char1 VALUE 'D',
      END OF mc_cud_type .
  constants MC_KEY_FIELD type STRING value 'ABRUFNUMMER' ##NO_TEXT.

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
      !IS_OMESSAGECONTAINER type ref to /IWBEP/IF_MESSAGE_CONTAINER optional
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
  methods GENERATE_ABRUF_NUMMER
    importing
      !IS_NR_RANGE type INRI-NRRANGENR default ZIF_RS_3CONSTANTS=>GC_ABRUF_NR_RANGE
      !IS_OBJECT type INRI-OBJECT default ZIF_RS_3CONSTANTS=>GC_ABRUF_NR_OBJECT
    returning
      value(RV_NUMBER) type STRING
    raising
      /IWBEP/CX_MGW_TECH_EXCEPTION
      /IWBEP/CX_MGW_BUSI_EXCEPTION .
  methods SUBMIT_DB_VALUES
    importing
      !IS_T_ABRUFKOPF type TT_T_ABRUFKOPF
    raising
      /IWBEP/CX_MGW_TECH_EXCEPTION
      /IWBEP/CX_MGW_BUSI_EXCEPTION .
  methods COPY_DATA_TO_REF
    importing
      !IS_DATA type ANY
    changing
      !CS_DATA type ref to DATA
    raising
      /IWBEP/CX_MGW_TECH_EXCEPTION
      /IWBEP/CX_MGW_BUSI_EXCEPTION .
  methods MAP_ENTITY_TO_DB_STRUC
    importing
      !IS_ER_ENTITY type TS_INPUT_PARAMETERS-ER_ENTITY
    returning
      value(RV_ER_ENTITY) type TS_S_ABRUFKOPF
    raising
      /IWBEP/CX_MGW_TECH_EXCEPTION
      /IWBEP/CX_MGW_BUSI_EXCEPTION .
  methods SET_STATUS_DESCRIPTIONS
    importing
      !IS_ET_ENTITY type TS_INPUT_PARAMETERS-ET_ENTITYSET
    returning
      value(RV_ET_ENTITY) type TS_INPUT_PARAMETERS-ET_ENTITYSET
    raising
      /IWBEP/CX_MGW_TECH_EXCEPTION
      /IWBEP/CX_MGW_BUSI_EXCEPTION .
  methods SET_VORHABEN
    raising
      /IWBEP/CX_MGW_TECH_EXCEPTION
      /IWBEP/CX_MGW_BUSI_EXCEPTION .
  methods GET_INDIVIDUAL_CONTRACT_DESCR
    importing
      !IS_VALUE type TS_ABRUFKOPF-EINZELVERTRAG
    returning
      value(RV_DESCR) type TS_ABRUFKOPF-INDIVIDUAL_CONTRACT_DESC
    raising
      /IWBEP/CX_MGW_TECH_EXCEPTION
      /IWBEP/CX_MGW_BUSI_EXCEPTION .
  methods GET_VERRECHNUNGSELEMENT_DESCR
    importing
      !IS_VALUE type TS_ABRUFKOPF-VERRECHNUNGSELEMENT
      !IS_TYPE type TS_ABRUFKOPF-TYP_VERRECHNUNGSELEMENT
    returning
      value(RV_DESCR) type TS_ABRUFKOPF-ACCT_ELEMENT_DESC
    raising
      /IWBEP/CX_MGW_TECH_EXCEPTION
      /IWBEP/CX_MGW_BUSI_EXCEPTION .
  methods GET_TYPVERRECHNUNGS_DESCR
    importing
      !IS_VALUE type TS_ABRUFKOPF-TYP_VERRECHNUNGSELEMENT
    returning
      value(RV_DESCR) type TS_ABRUFKOPF-ACCT_ELEMENT_TYPE_DESC
    raising
      /IWBEP/CX_MGW_TECH_EXCEPTION
      /IWBEP/CX_MGW_BUSI_EXCEPTION .
  methods GET_STATUS_DESCR
    importing
      !IS_VALUE type TS_ABRUFKOPF-STATUS
      !IS_LANGU type SPRAS default SY-LANGU
      !IS_PROZESSART type ZRS_3TT_DE_PROZESSART default 'V'
    returning
      value(RV_DESCR) type TS_ABRUFKOPF-STATUS_DESC
    raising
      /IWBEP/CX_MGW_TECH_EXCEPTION
      /IWBEP/CX_MGW_BUSI_EXCEPTION .
  methods GET_STATUSET
    importing
      !IS_STATUS type TS_ABRUFKOPF-STATUS_ABRUF
    returning
      value(RV_T_STATUS) type ZCL_RS_3PRJABRSTAT=>TT_PRJABRST
    raising
      /IWBEP/CX_MGW_TECH_EXCEPTION
      /IWBEP/CX_MGW_BUSI_EXCEPTION .
  methods SET_FOR_UPDATE
    importing
      !IS_ET_ENTITY type TS_INPUT_PARAMETERS-ET_ENTITYSET
    returning
      value(RV_ET_ENTITY) type TS_INPUT_PARAMETERS-ET_ENTITYSET
    raising
      /IWBEP/CX_MGW_TECH_EXCEPTION
      /IWBEP/CX_MGW_BUSI_EXCEPTION .
  methods GET_STATUSABRUF_DESCR
    importing
      !IS_VALUE type TS_ABRUFKOPF-STATUS_ABRUF
      !IS_PROZESSART type ZCL_RS_3PRJABRSTAT=>TS_PRJABRST-PROZESSART default ZIF_RS_3CONSTANTS=>GC_PROCEZZART_TYPE_ABRUF
    returning
      value(RV_DESCR) type TS_ABRUFKOPF-STATUS_DESC
    raising
      /IWBEP/CX_MGW_TECH_EXCEPTION
      /IWBEP/CX_MGW_BUSI_EXCEPTION .
  methods SET_DEEPSTRUC_DETAILS
    importing
      !IS_ET_ENTITY type TS_INPUT_PARAMETERS-ET_ENTITYSET
    returning
      value(RV_ET_DEEPENTITY) type TT_DEEP_ABRUFKOPF
    raising
      /IWBEP/CX_MGW_TECH_EXCEPTION
      /IWBEP/CX_MGW_BUSI_EXCEPTION .
  methods HANDLE_ABRUFKOPF
    importing
      !IS_ER_ENTITY type TS_INPUT_PARAMETERS-ER_ENTITY
    returning
      value(RV_ER_ENTITY) type TS_INPUT_PARAMETERS-ER_ENTITY
    raising
      /IWBEP/CX_MGW_TECH_EXCEPTION
      /IWBEP/CX_MGW_BUSI_EXCEPTION .
ENDCLASS.



CLASS ZCL_RS_3ABRUFKOPF IMPLEMENTATION.


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
    DATA(lo_abruf) = NEW zcl_rs_3abrufkopf( is_params ).
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
    DATA(lv_mandt) = sy-mandt.

    CONDENSE lv_osql_where_clause.
    IF lv_osql_where_clause CS mc_filter_values-is_edit_trueformat.
      REPLACE ALL OCCURRENCES: OF mc_filter_values-is_edit_trueformat IN lv_osql_where_clause WITH mc_filter_values-mandt_filter,
                               OF mc_filter_values-ampersand IN lv_osql_where_clause WITH lv_mandt.
    ENDIF.

    DATA(lt_entity) = get_db_values( lv_osql_where_clause ).
    lt_entity = set_for_update( lt_entity ).
    lt_entity = set_status_descriptions( lt_entity ).
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
            vorhabensnummer
            abgerufen_durch
            abgerufen_am
            einzelvertrag
            verrechnungselement
            typ_verrechnungselement
            status_abruf
            notiz
            lastchangedby
            lastchangedon
            projektleiter
            stv_projektleiter
            software_architekt
            stv_swa
            service_spoc
            stv_sspoc
            status
          FROM zrs_d_3tt_abrufv
           INTO TABLE rv_et_entity
           WHERE (lv_osql_where_clause).
    IF rv_et_entity IS NOT INITIAL.
      RETURN.
    ENDIF.
  ENDMETHOD.


  METHOD get_individual_contract_descr.
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
    DATA(lv_value) = is_value.
    CLEAR rv_descr.
    DATA ls_entity TYPE zcl_rs_3domain_einzelvertrag=>ts_input_parameters-er_entity.
    DATA(lo_entitat) = zcl_rs_3domain_einzelvertrag=>create(
           VALUE zcl_rs_3domain_einzelvertrag=>ts_input_parameters(
                             it_key_tab  = VALUE /iwbep/t_mgw_name_value_pair( ( name = 'CODE'
                                                            value = lv_value ) )
                             er_entity = ls_entity
                             ) ).
    lo_entitat->read( ).
    DATA(lo_antwort) = CAST zif_rs_3dpc_antwort( lo_entitat ).
    lo_antwort->get_response_read( IMPORTING es_entity = ls_entity
                                             es_response_context = DATA(es_response) ).
    IF ls_entity IS NOT INITIAL.
      rv_descr = CONV ts_abrufkopf-individual_contract_desc( ls_entity-beschreibung ).
    ENDIF.
  ENDMETHOD.


  METHOD get_statusabruf_descr.
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
    DATA(lv_value) = is_value.
    DATA(lv_prozessart) = is_prozessart.

    DATA lv_statusdescr LIKE rv_descr.
    CLEAR rv_descr.
    IF lv_value IS INITIAL.
      RETURN.
    ENDIF.
    DATA ls_input_params TYPE zcl_rs_3prjabrstat=>ts_input_parameters.
    DATA(lo_prjabrstat) = NEW zcl_rs_3prjabrstat( ls_input_params ).
    DATA(lv_osql_where_clause) = mc_filter_values-prjabrstat_filter.
    REPLACE ALL OCCURRENCES: OF mc_filter_values-at_sign IN lv_osql_where_clause WITH lv_prozessart,
                             OF mc_filter_values-ampersand IN lv_osql_where_clause WITH lv_value.
    DATA(lt_entity) = lo_prjabrstat->get_db_values( lv_osql_where_clause ).
    IF lt_entity IS NOT INITIAL.
      lv_statusdescr = lt_entity[ 1 ]-statusbeschreibung.
    ENDIF.
    IF lv_statusdescr IS NOT INITIAL.
      rv_descr = lv_statusdescr.
    ENDIF.
  ENDMETHOD.


  METHOD get_statuset.
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
    REFRESH rv_t_status.
    DATA ls_input TYPE zcl_rs_3prjabrstat=>ts_input_parameters.
    DATA(lv_input) = is_status.
    DATA(lo_status) = NEW zcl_rs_3prjabrstat( ls_input ).
    DATA(lv_osql_where_clause) = lo_status->mc_filter_values-to_read_stat.
    DATA(lv_langu) = sy-langu.
    DATA lt_status_range LIKE zcl_rs_3prjabrstat=>gt_status.

    REPLACE ALL OCCURRENCES: OF lo_status->mc_filter_values-ampersand IN lv_osql_where_clause WITH lv_langu,
                             OF lo_status->mc_filter_values-at_sign IN lv_osql_where_clause WITH zif_rs_3constants=>gc_procezzart_type_abruf,
                             OF lo_status->mc_filter_values-asterisk IN lv_osql_where_clause WITH lv_input.
    CONDENSE lv_osql_where_clause.
    DATA(lt_entity) = lo_status->get_db_values( lv_osql_where_clause ).

    IF lt_entity IS INITIAL.
      RETURN.
    ENDIF.

    lt_status_range = VALUE #( FOR ls_prjabr IN lt_entity
                  ( sign = zif_rs_3constants=>gc_range_sign_include
                    option = COND #( WHEN ls_prjabr-hsonr IS NOT INITIAL THEN zif_rs_3constants=>gc_range_option_between
                                     ELSE zif_rs_3constants=>gc_range_option_equal )
                    low = ls_prjabr-nsonr
                    high = ls_prjabr-hsonr )
                  ).

    IF lt_status_range IS NOT INITIAL.
      REFRESH lo_status->gt_status.
      lo_status->gt_status = lt_status_range.
      CLEAR lv_osql_where_clause.
      lv_osql_where_clause = lo_status->mc_filter_values-to_read_multstat.
      REPLACE ALL OCCURRENCES: OF lo_status->mc_filter_values-ampersand IN lv_osql_where_clause WITH lv_langu,
                               OF lo_status->mc_filter_values-at_sign IN lv_osql_where_clause WITH zif_rs_3constants=>gc_procezzart_type_abruf.
      DATA(lt_subentity) = lo_status->get_db_values( lv_osql_where_clause ).
      IF lt_subentity IS NOT INITIAL.
        APPEND LINES OF lt_subentity TO lt_entity.
        REFRESH lt_subentity.
      ENDIF.
    ENDIF.
    SORT lt_entity BY status.
    DELETE ADJACENT DUPLICATES FROM lt_entity COMPARING status.
    rv_t_status = lt_entity.
  ENDMETHOD.


  METHOD get_status_descr.
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
    DATA(lv_value) = is_value.
    DATA(lv_langu) = is_langu.
    IF lv_langu  IS INITIAL.
      lv_langu = sy-langu.
    ENDIF.
    DATA(lv_prozessart) = is_prozessart.
    CLEAR rv_descr.
    IF lv_value IS INITIAL.
      RETURN.
    ENDIF.

    DATA lv_status TYPE zrs_3tt_de_status.
    lv_status = lv_value.
    DATA(lo_vorhaben) = NEW zcl_rs_3vorhaben( ).

    lo_vorhaben->get_status_text(
           EXPORTING
             iv_sprache            = lv_langu
             iv_prozessart         = lv_prozessart
             iv_status             = lv_status
           IMPORTING
             ev_statusbeschreibung = DATA(lv_statusdescr) ).

    IF lv_statusdescr IS NOT INITIAL.
      rv_descr = lv_statusdescr.
    ENDIF.
  ENDMETHOD.


  METHOD get_typverrechnungs_descr.
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
    DATA(lv_value) = is_value.
    CLEAR rv_descr.
    IF lv_value IS INITIAL.
      RETURN.
    ENDIF.
    DATA lv_type TYPE zrs_3tt_de_typverchel.
    lv_type = lv_value.
    DATA(lo_vorhaben) = NEW zcl_rs_3vorhaben( ).

    lo_vorhaben->get_acct_elem_type_text(
           EXPORTING
             iv_acct_elem_type = lv_type
           IMPORTING
             ev_text           = DATA(ls_descr) ).

    IF ls_descr IS NOT INITIAL.
      rv_descr = ls_descr.
    ENDIF.
  ENDMETHOD.


  METHOD get_verrechnungselement_descr.
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
    DATA(lv_value) = is_value.
    DATA(lv_type) = is_type.
    CLEAR rv_descr.
    IF lv_value IS INITIAL.
      RETURN.
    ENDIF.
    IF lv_type IS INITIAL.
      RETURN.
    ENDIF.
    DATA(lt_filter_select_options) = VALUE /iwbep/t_mgw_select_option(
                                                                       ( property = 'TYPE'
                                                                         select_options = VALUE #(
                                                                                                    ( low = lv_type )
                                                                                                  )
                                                                        )
                                                                       ( property = 'KONT_ELEM_ID'
                                                                         select_options = VALUE #(
                                                                                                    ( low = lv_value )
                                                                                                  )
                                                                        )
                                                                    ).
    DATA(lo_vorhaben) = NEW zcl_rs_3vorhaben( ).

    lo_vorhaben->get_accounting_element(
      EXPORTING
        it_filter_select_options = lt_filter_select_options
      IMPORTING
        et_entityset             = DATA(lt_entity)
        et_return                = DATA(lt_return) ).

    IF lt_entity IS NOT INITIAL.
      rv_descr = lt_entity[ 1 ]-kont_elem_descr.
    ENDIF.
  ENDMETHOD.


  METHOD handle_abrufkopf.
    DATA(ls_er_entity) = is_er_entity.
    DATA lt_entity  TYPE tt_t_abrufkopf.

    rv_er_entity = ls_er_entity.

    IF ls_er_entity IS INITIAL.
      RETURN.
    ENDIF.

    APPEND map_entity_to_db_struc( ls_er_entity ) TO lt_entity.
    submit_db_values( lt_entity ).

    rv_er_entity = ls_er_entity.

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
    CLEAR rv_er_entity.
    rv_er_entity = VALUE ts_s_abrufkopf(
                              mandt = is_er_entity-mandt
                              abrufnummer = is_er_entity-abrufnummer
                              vorhabensnummer = is_er_entity-vorhabensnummer
                              abgerufen_durch = is_er_entity-abgerufen_durch
                              abgerufen_am = is_er_entity-abgerufen_am
                              einzelvertrag = is_er_entity-einzelvertrag
                              verrechnungselement = is_er_entity-verrechnungselement
                              typ_verrechnungselement = is_er_entity-typ_verrechnungselement
                              status_abruf = is_er_entity-status_abruf
                              notiz = is_er_entity-notiz
                              lastchangedby = is_er_entity-lastchangedby
                              lastchangedon = is_er_entity-lastchangedon
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


  METHOD set_deepstruc_details.
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
    DATA(lt_entity) = is_et_entity.
    REFRESH rv_et_deepentity.
    IF lt_entity IS INITIAL.
      RETURN.
    ENDIF.

    DATA(lt_sub_entity) = VALUE tt_deep_abrufkopf( FOR ls_entity IN lt_entity
                           (
                             mandt = ls_entity-mandt
                             abrufnummer = ls_entity-abrufnummer
                             vorhabensnummer = ls_entity-vorhabensnummer
                             abgerufen_durch = ls_entity-abgerufen_durch
                             abgerufen_am = ls_entity-abgerufen_am
                             einzelvertrag = ls_entity-einzelvertrag
                             verrechnungselement = ls_entity-verrechnungselement
                             typ_verrechnungselement = ls_entity-typ_verrechnungselement
                             status_abruf = ls_entity-status_abruf
                             notiz = ls_entity-notiz
                             lastchangedby = ls_entity-lastchangedby
                             lastchangedon = ls_entity-lastchangedon
                             projektleiter = ls_entity-projektleiter
                             stv_projektleiter = ls_entity-stv_projektleiter
                             software_architekt = ls_entity-software_architekt
                             stv_swa = ls_entity-stv_swa
                             service_spoc = ls_entity-service_spoc
                             stv_sspoc = ls_entity-stv_sspoc
                             status = ls_entity-status
                             status_desc = get_statusabruf_descr( is_value = ls_entity-status_abruf )
   acct_element_type_desc = get_typverrechnungs_descr( ls_entity-typ_verrechnungselement )
   acct_element_desc = get_verrechnungselement_descr( is_value = ls_entity-verrechnungselement
                                                      is_type = ls_entity-typ_verrechnungselement )
   individual_contract_desc = get_individual_contract_descr( ls_entity-einzelvertrag )
                                  tostatusset = get_statuset( ls_entity-status_abruf )
     )
    ).

    IF lt_sub_entity IS NOT INITIAL.
      REFRESH rv_et_deepentity.
      rv_et_deepentity = lt_sub_entity.
    ENDIF.
  ENDMETHOD.


  METHOD set_for_update.
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
    DATA(lt_entity) = is_et_entity.
    rv_et_entity = lt_entity.
    IF lt_entity IS INITIAL.
      RETURN.
    ENDIF.
    DATA(lv_uname) = sy-uname.
    DATA(ls_entity) = lt_entity[ 1 ].

    DATA(lt_filter_select_options) = gs_params-io_tech_request_context_query->get_filter( )->get_filter_select_options( ).
    IF line_exists( lt_filter_select_options[ property = mc_filter_values-is_edit_value ] ).
      DATA(lt_sel_opts) = lt_filter_select_options[ property = mc_filter_values-is_edit_value ]-select_options.
      IF lt_sel_opts IS NOT INITIAL.
        DATA(lv_boolean) = lt_sel_opts[ 1 ]-low.
      ENDIF.
    ELSE.
      RETURN.
    ENDIF.

    IF lv_boolean <> abap_true.
*   set timestamp
      DATA lv_timestamp_reset TYPE timestamp.
      GET TIME STAMP FIELD lv_timestamp_reset.
      DATA(lv_secs) = zif_rs_3constants=>gc_session_expires_secs-header * zif_rs_3constants=>gc_session_expires_secs-multiplier.

      lv_timestamp_reset = cl_abap_tstmp=>subtractsecs( tstmp  = lv_timestamp_reset
                                                        secs = lv_secs ).
      ls_entity-lastchangedon = lv_timestamp_reset.

*    release the locking
      DATA lt_abrufkopf TYPE tt_t_abrufkopf.
      APPEND map_entity_to_db_struc( ls_entity ) TO lt_abrufkopf.
      submit_db_values( lt_abrufkopf ).

      IF ls_entity-abrufnummer IS NOT INITIAL.
          DATA(lv_osql_where_clause) = mc_filter_values-abrufnummer_filter.
          REPLACE ALL OCCURRENCES: OF mc_filter_values-ampersand IN lv_osql_where_clause WITH ls_entity-abrufnummer.
          CONDENSE lv_osql_where_clause.

          lt_entity = get_db_values( lv_osql_where_clause ).
      ENDIF.

      IF lt_entity IS NOT INITIAL.
        REFRESH rv_et_entity.
        rv_et_entity = lt_entity.
      ENDIF.
      RETURN.
    ENDIF.

* set timstamps
    DATA lv_timestamp_now TYPE timestamp.
    GET TIME STAMP FIELD lv_timestamp_now.

    IF ls_entity-lastchangedon IS NOT INITIAL.
      DATA(lv_timestamp_current) = ls_entity-lastchangedon.
    ELSE.
      lv_timestamp_current =  lv_timestamp_now.
    ENDIF.

    IF lv_timestamp_current <> lv_timestamp_now
     AND ls_entity-lastchangedby <> lv_uname.
      DATA(lv_diffsecs) = cl_abap_tstmp=>subtract( tstmp1 = lv_timestamp_now
                                                   tstmp2 = lv_timestamp_current ).
      DATA lv_session_time TYPE tzntstmpl.
      lv_session_time = CONV tzntstmpl( zif_rs_3constants=>gc_session_expires_secs-header ).
      IF lv_diffsecs <  lv_session_time.
        raise_error( is_message = |Wird derzeit von bearbeitet { ls_entity-lastchangedby }| ). " Currently being edited by
      ENDIF.
    ENDIF.

    ls_entity-lastchangedby = lv_uname.
    ls_entity-lastchangedon = lv_timestamp_now.
    ls_entity-is_edit = abap_true.

    APPEND map_entity_to_db_struc( ls_entity ) TO lt_abrufkopf.
    submit_db_values( lt_abrufkopf ).

    IF ls_entity-abrufnummer IS NOT INITIAL.
      lv_osql_where_clause = mc_filter_values-abrufnummer_filter.
      REPLACE ALL OCCURRENCES: OF mc_filter_values-ampersand IN lv_osql_where_clause WITH ls_entity-abrufnummer.
      CONDENSE lv_osql_where_clause.

      lt_entity = get_db_values( lv_osql_where_clause ).
    ENDIF.

    IF lt_entity IS NOT INITIAL.
      REFRESH rv_et_entity.
      rv_et_entity = lt_entity.
    ENDIF.
  ENDMETHOD.


  METHOD set_status_descriptions.
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
    DATA(lt_entity) = is_et_entity.
    rv_et_entity = lt_entity.
    IF lt_entity IS INITIAL.
      RETURN.
    ENDIF.

    DATA(lt_sub_entity) = VALUE ts_input_parameters-et_entityset( FOR ls_entity IN lt_entity
                            (
                              mandt = ls_entity-mandt
                              abrufnummer = ls_entity-abrufnummer
                              vorhabensnummer = ls_entity-vorhabensnummer
                              abgerufen_durch = ls_entity-abgerufen_durch
                              abgerufen_am = ls_entity-abgerufen_am
                              einzelvertrag = ls_entity-einzelvertrag
                              verrechnungselement = ls_entity-verrechnungselement
                              typ_verrechnungselement = ls_entity-typ_verrechnungselement
                              status_abruf = ls_entity-status_abruf
                              notiz = ls_entity-notiz
                              lastchangedby = ls_entity-lastchangedby
                              lastchangedon = ls_entity-lastchangedon
                              projektleiter = ls_entity-projektleiter
                              stv_projektleiter = ls_entity-stv_projektleiter
                              software_architekt = ls_entity-software_architekt
                              stv_swa = ls_entity-stv_swa
                              service_spoc = ls_entity-service_spoc
                              stv_sspoc = ls_entity-stv_sspoc
                              status = ls_entity-status
                              status_desc = get_statusabruf_descr( is_value = ls_entity-status_abruf )
    acct_element_type_desc = get_typverrechnungs_descr( ls_entity-typ_verrechnungselement )
    acct_element_desc = get_verrechnungselement_descr( is_value = ls_entity-verrechnungselement
                                                       is_type = ls_entity-typ_verrechnungselement )
    individual_contract_desc = get_individual_contract_descr( ls_entity-einzelvertrag )
                              is_edit = ls_entity-is_edit
  )
 ).

    IF lt_sub_entity IS NOT INITIAL.
      REFRESH rv_et_entity.
      rv_et_entity = lt_sub_entity.
    ENDIF.
  ENDMETHOD.


  METHOD set_vorhaben.
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
    DATA(lv_isupdate) = abap_false.
    DATA(lv_condition) = mc_filter_values-to_read_vorhaben.
    REPLACE ALL OCCURRENCES: OF mc_filter_values-ampersand IN lv_condition WITH gs_params-er_entity-vorhabensnummer,
                             OF mc_filter_values-at_sign IN lv_condition WITH zif_rs_3constants=>gc_vorhaben_status_active.

    DATA(lo_projects) = NEW zcl_rs_3vorhaben(  ).
    lo_projects->get_all_projects(  EXPORTING iv_condition = lv_condition
                                     IMPORTING et_vorhaben = DATA(lt_vorhaben_entity) ).

    IF lt_vorhaben_entity IS NOT INITIAL.
      DATA(ls_vorhaben) = lt_vorhaben_entity[ 1 ].
      IF ls_vorhaben-typ_verrechnungselement <> gs_params-er_entity-typ_verrechnungselement.
        ls_vorhaben-typ_verrechnungselement = gs_params-er_entity-typ_verrechnungselement.
        lv_isupdate = abap_true.
      ENDIF.
      IF ls_vorhaben-verrechnungselement <> gs_params-er_entity-verrechnungselement.
        ls_vorhaben-typ_verrechnungselement = gs_params-er_entity-typ_verrechnungselement.
        lv_isupdate = abap_true.
      ENDIF.
      IF lv_isupdate = abap_true.
        TYPES ts_zrs_3tt_vorhaben TYPE zrs_3tt_vorhaben.
        DATA(ls_input_vorhaben) = VALUE ts_zrs_3tt_vorhaben(
                                            mandt  = ls_vorhaben-mandt
                                            vorhabensnummer	= ls_vorhaben-vorhabensnummer
                                            ppm_nummer  = ls_vorhaben-ppm_nummer
                                            projektleiter	= ls_vorhaben-projektleiter
                                            stv_projektleiter	= ls_vorhaben-stv_projektleiter
                                            software_architekt  = ls_vorhaben-software_architekt
                                            stv_swa	= ls_vorhaben-stv_swa
                                            projekt_forecast_aw	= ls_vorhaben-projekt_forecast_aw
                                            projekt_forecast_tp	= ls_vorhaben-projekt_forecast_tp
                                            service_spoc  = ls_vorhaben-service_spoc
                                            stv_sspoc	= ls_vorhaben-stv_sspoc
                                            funktionaler_bereich  = ls_vorhaben-funktionaler_bereich
                                            verrechnungselement	= ls_vorhaben-verrechnungselement
                                            typ_verrechnungselement	= ls_vorhaben-typ_verrechnungselement
                                            projektbezeichnung  = ls_vorhaben-projektbezeichnung
                                            kommentar	= ls_vorhaben-kommentar
                                            projekt_start	= ls_vorhaben-projekt_start
                                            projekt_ende  = ls_vorhaben-projekt_ende
                                            entwicklungsstart	= ls_vorhaben-entwicklungsstart
                                            status  = ls_vorhaben-status
                                            ).
        lo_projects->update_project( EXPORTING is_project = ls_input_vorhaben
                                     IMPORTING et_return = DATA(lt_return) ).
        IF NOT line_exists( lt_return[ type = zif_rs_3constants=>gc_message_type_success ] ).
          MESSAGE i052(zrs_3) INTO DATA(lv_message).
          raise_error( is_message = lv_message ). "Vorhaben Table was not updated successfully
        ENDIF.
      ENDIF.
    ENDIF.
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
    DATA(lt_abrufkopf) = is_t_abrufkopf.
    DATA lv_message TYPE string.

    IF lines( lt_abrufkopf ) >= 1.
      "insert value/update data
      MODIFY zrs_3tt_abrufkop FROM TABLE lt_abrufkopf.
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
    es_mo_context = gs_params-mo_context.
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
    es_mo_context = gs_params-mo_context.
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
    es_mo_context = gs_params-mo_context.
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
    es_entityset = gs_params-er_entityset_expand.
    es_t_expanded_clauses = gs_params-et_expanded_clauses.
    es_t_expanded_tech_clauses = gs_params-et_expanded_tech_clauses .
    es_response_context = gs_params-es_response_context_set.
    es_mo_context = gs_params-mo_context.
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
    es_entity = gs_params-er_entity_expand.
    es_response_context = gs_params-es_response_context_read.
    es_t_expanded_clauses = gs_params-et_expanded_clauses.
    es_t_expanded_tech_clauses = gs_params-et_expanded_tech_clauses.
    es_mo_context = gs_params-mo_context.
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
    es_mo_context = gs_params-mo_context.
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
          lv_message TYPE string.

    DATA(lv_uname) = sy-uname.
    IF gs_params-io_data_provider IS BOUND.
      gs_params-io_data_provider->read_entry_data( IMPORTING es_data = ls_entity ).
    ENDIF.

    IF ls_entity IS NOT INITIAL.
      gs_params-er_entity = ls_entity.
    ENDIF.

    IF gs_params-er_entity IS INITIAL.
      RETURN.
    ENDIF.

* Check status and field values(based on status)
    DATA(lo_status) = zcl_rs_3abrufstatus=>create( VALUE zif_rs_3dpc_abrufstatus=>ts_input_parameters(
                                                                     iv_entity_set_name = gs_params-iv_entity_set_name
                                                                     is_cud_type = zif_rs_3constants=>gc_cud_types-create
                                                                     er_entity_kopf = gs_params-er_entity
                                                                     es_mo_context = gs_params-mo_context ) ).
    lo_status->check( ).
    lo_status->get_results( IMPORTING es_entity_vorhaben = DATA(ls_vorhaben)
                                      es_entity_kopf = DATA(ls_entity_kopf)
                                      es_entity_kopf_old = DATA(ls_entity_kopf_old_)
                                      es_mo_context = gs_params-mo_context
                                      es_email_params = DATA(ls_email_params) ).

* Save data
*    set_vorhaben( ).
    gs_params-er_entity = handle_abrufkopf( ls_entity_kopf ).

    DATA(lo_response) = NEW zcl_rs_3abrufkopf( gs_params ).
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
    DATA(lo_response) = NEW zcl_rs_3abrufkopf( gs_params ).
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

    DATA(lv_mandt) = sy-mandt.

    DATA(lv_osql_where_clause) = gs_params-io_tech_request_context_query->get_osql_where_clause( ).
    IF lv_osql_where_clause CS mc_filter_values-is_edit_trueformat.
      REPLACE ALL OCCURRENCES: OF mc_filter_values-is_edit_trueformat IN lv_osql_where_clause WITH mc_filter_values-mandt_filter,
                               OF mc_filter_values-ampersand IN lv_osql_where_clause WITH lv_mandt.
    ENDIF.

    APPEND gs_params-io_tech_request_context_query->get_expand( ) TO gs_params-et_expanded_tech_clauses.

    DATA(lt_entity) = get_db_values( lv_osql_where_clause ).
    lt_entity = set_for_update( lt_entity ).

    DATA(lt_deep_entity) = set_deepstruc_details( lt_entity ).

    IF lt_deep_entity  IS NOT INITIAL.
      copy_data_to_ref(
         EXPORTING
          is_data = lt_deep_entity
         CHANGING
          cs_data = gs_params-er_entityset_expand ).
    ENDIF.

    DATA(lo_response) = NEW zcl_rs_3abrufkopf( gs_params ).
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

    DATA(lv_osql_where_clause) = mc_filter_values-abrufnummer_filter.
    DATA(lt_keys) = gs_params-io_tech_request_context_read->get_keys( ).
    IF NOT line_exists( lt_keys[ name = mc_key_field ] ) OR lt_keys IS INITIAL.
      RETURN.
    ENDIF.

    DATA(lv_abrufnummer) = lt_keys[ name = mc_key_field ]-value.
    REPLACE ALL OCCURRENCES: OF mc_filter_values-ampersand IN lv_osql_where_clause WITH lv_abrufnummer.
    CONDENSE lv_osql_where_clause.

    DATA(lt_entity) = get_db_values( lv_osql_where_clause ).
    lt_entity = set_status_descriptions( lt_entity ).
    IF lt_entity IS NOT INITIAL.
      gs_params-er_entity = lt_entity[ 1 ].
    ENDIF.

    DATA(lo_response) = NEW zcl_rs_3abrufkopf( gs_params ).
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
    IF gs_params-iv_entity_name <> gs_params-iv_source_name.
      RETURN.
    ENDIF.

    DATA(lo_expand) = CAST /iwbep/cl_mgw_expand_node( gs_params-io_expand ).
    APPEND lo_expand->get_expand( ) TO gs_params-et_expanded_tech_clauses.

    DATA(lv_osql_where_clause) = mc_filter_values-abrufnummer_filter.
    DATA(lt_keys) = gs_params-io_tech_request_context_read->get_keys( ).
    IF NOT line_exists( lt_keys[ name = mc_key_field ] ) OR lt_keys IS INITIAL.
      RETURN.
    ENDIF.

    DATA(lv_abrufnummer) = lt_keys[ name = mc_key_field ]-value.
    REPLACE ALL OCCURRENCES: OF mc_filter_values-ampersand IN lv_osql_where_clause WITH lv_abrufnummer.
    CONDENSE lv_osql_where_clause.

    DATA(lt_entity) = get_db_values( lv_osql_where_clause ).

    DATA(lt_deep_entity) = set_deepstruc_details( lt_entity ).

    IF lt_deep_entity  IS NOT INITIAL.
      DATA(ls_deep_entity) = lt_deep_entity[ 1 ].
      copy_data_to_ref(
         EXPORTING
          is_data = ls_deep_entity
         CHANGING
          cs_data = gs_params-er_entity_expand ).
    ENDIF.

    DATA(lo_response) = NEW zcl_rs_3abrufkopf( gs_params ).
    rv_if_response = lo_response.
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
    DATA: ls_entity LIKE gs_params-er_entity.

    gs_params-io_data_provider->read_entry_data( IMPORTING es_data = ls_entity ).
    gs_params-er_entity = ls_entity.
    IF ls_entity IS INITIAL.
      RETURN.
    ENDIF.

* Check status and field values(based on status)
    DATA(lo_status) = zcl_rs_3abrufstatus=>create( VALUE zif_rs_3dpc_abrufstatus=>ts_input_parameters(
                                                                     iv_entity_set_name = gs_params-iv_entity_set_name
                                                                     is_cud_type = zif_rs_3constants=>gc_cud_types-update
                                                                     er_entity_kopf = gs_params-er_entity
                                                                     es_mo_context = gs_params-mo_context ) ).
    lo_status->check( ).
    lo_status->get_results( IMPORTING es_entity_vorhaben = DATA(ls_vorhaben)
                                      es_entity_kopf = DATA(ls_entity_kopf)
                                      es_entity_kopf_old = DATA(ls_entity_kopf_old_)
                                      es_mo_context = gs_params-mo_context
                                      es_email_params = DATA(ls_email_params) ).
* Save data
*    set_vorhaben( ).
    gs_params-er_entity = handle_abrufkopf( ls_entity_kopf ).

    DATA(lo_response) = NEW zcl_rs_3abrufkopf( gs_params ).
    rv_if_response = lo_response.
  ENDMETHOD.
ENDCLASS.
