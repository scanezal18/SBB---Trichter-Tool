CLASS zcl_rs_3mdartikel DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_rs_3dpc_antwort .
    INTERFACES zif_rs_3dpc_entitat .

    TYPES ts_mdartikel TYPE zrs_3tt_mdartikel_s .
    TYPES:
      tt_mdartikel TYPE STANDARD TABLE OF ts_mdartikel WITH DEFAULT KEY .
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
        er_entity                      TYPE ts_mdartikel,
        et_entityset                   TYPE tt_mdartikel,
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
    DATA: BEGIN OF gs_deep_mdartikel.
            INCLUDE TYPE ts_mdartikel.
            DATA: tokatalogpreis TYPE zcl_rs_3katalogpreis=>tt_katalogpreis,
          END OF gs_deep_mdartikel .

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

  types TS_S_MDARTIKEL type ZRS_3TT_MDARTIKL .
  types:
    tt_t_mdartikel TYPE STANDARD TABLE OF ts_s_mdartikel WITH DEFAULT KEY .
  types TS_S_MDARTTEXT type ZRS_3TT_MDARTIKT .
  types:
    tt_t_mdarttext TYPE STANDARD TABLE OF ts_s_mdarttext WITH DEFAULT KEY .
  types:
    tt_deep_mdartikel LIKE STANDARD TABLE OF gs_deep_mdartikel WITH DEFAULT KEY .

  data GS_PARAMS type TS_INPUT_PARAMETERS .
  constants MC_VERBINDER type CHAR2 value '=>' ##NO_TEXT.
  constants:
    BEGIN OF mc_filter_values,
        sprache_filter      TYPE string VALUE '( Sprache eq `&` )' ##NO_TEXT,
        mandt_filter        TYPE string VALUE '( Mandt eq `&` )' ##NO_TEXT,
        and                 TYPE string VALUE 'and' ##NO_TEXT,
        or                  TYPE string VALUE 'or' ##NO_TEXT,
        at_sign             TYPE string VALUE '@' ##NO_TEXT,
        ampersand           TYPE string VALUE '&' ##NO_TEXT,
        asterisk            TYPE string VALUE '*' ##NO_TEXT,
        to_read             TYPE string VALUE '( Artikelnummer eq `&` )' ##NO_TEXT,
        to_serviceobj       TYPE string VALUE '( Nummer_lieferobjekt eq `&` )' ##NO_TEXT,
        to_complexity       TYPE string VALUE '( Nummer_komplexitaet eq `&` )' ##NO_TEXT,
        to_category         TYPE string VALUE '( Nummer_kategorie eq `&` )' ##NO_TEXT,
        to_unit             TYPE string VALUE '( Code  eq `&` )' ##NO_TEXT,
        to_contract         TYPE string VALUE '( Code  eq `&` )' ##NO_TEXT,
        is_flag_trueformat  TYPE string VALUE `( FLAG = 'X' )`,
        is_flag_falseformat TYPE string VALUE `( FLAG = ' ' )`,
        to_katalogpreis     TYPE string VALUE '( EINHEIT eq `&` and ( GUELTIG_VON <= `@` and GUELTIG_BIS >= `@` ) )',
        to_create           TYPE string VALUE '( Artikelnummer eq `&` )' ##NO_TEXT,
        to_update           TYPE string VALUE '( Artikelnummer eq `&` and Gueltig_ab eq `*` )' ##NO_TEXT,
      END OF mc_filter_values .
  constants:
    BEGIN OF mc_error_opt,
        business  TYPE char1 VALUE 'B',
        technical TYPE char1 VALUE 'T',
      END OF mc_error_opt .
  constants:
    BEGIN OF mc_entry_type,
        timeslice TYPE char1 VALUE 'T',
        artikel   TYPE char1 VALUE 'A',
      END OF mc_entry_type .
  constants:
    BEGIN OF mc_cud_type,
        create TYPE char1 VALUE 'C',
        update TYPE char1 VALUE 'U',
        delete TYPE char1 VALUE 'D',
      END OF mc_cud_type .
  constants MC_KEY_FIELD type STRING value 'ARTIKELNUMMER' ##NO_TEXT.

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
  methods GET_DB_VALUES
    importing
      !IS_WHERE_CLAUSE type STRING default ''
    returning
      value(RV_ET_ENTITY) type TS_INPUT_PARAMETERS-ET_ENTITYSET
    raising
      /IWBEP/CX_MGW_TECH_EXCEPTION
      /IWBEP/CX_MGW_BUSI_EXCEPTION .
  methods GENERATE_ARTIKEL_NUMMER
    importing
      !IS_NR_RANGE type INRI-NRRANGENR default ZIF_RS_3CONSTANTS=>GC_ARTIKEL_NR_RANGE
      !IS_OBJECT type INRI-OBJECT default ZIF_RS_3CONSTANTS=>GC_ARTIKEL_NR_OBJECT
    returning
      value(RV_NUMBER) type STRING
    raising
      /IWBEP/CX_MGW_TECH_EXCEPTION
      /IWBEP/CX_MGW_BUSI_EXCEPTION .
  methods SUBMIT_DB_VALUES
    importing
      !IS_T_MDARTIKEL type TT_T_MDARTIKEL
      !IS_T_MDARTTEXT type TT_T_MDARTTEXT
    raising
      /IWBEP/CX_MGW_TECH_EXCEPTION
      /IWBEP/CX_MGW_BUSI_EXCEPTION .
  methods DEFINE_ARTIKEL_DETAILS
    importing
      !IS_CUD type CHAR1
      !IS_ER_ENTITY type TS_INPUT_PARAMETERS-ER_ENTITY
    exporting
      !ES_ENTRY_TYPE type CHAR1
      !ES_AKTUELL_ARTIKEL type TS_INPUT_PARAMETERS-ER_ENTITY
    raising
      /IWBEP/CX_MGW_TECH_EXCEPTION
      /IWBEP/CX_MGW_BUSI_EXCEPTION .
  methods VALIDATE_INPUT_VALUES
    importing
      !IS_CUD type CHAR1
      !IS_ER_ENTITY type TS_INPUT_PARAMETERS-ER_ENTITY
      !IS_ENTRY_TYPE type CHAR1
      !IS_AKTUELL_ARTIKEL type TS_INPUT_PARAMETERS-ER_ENTITY
    raising
      /IWBEP/CX_MGW_TECH_EXCEPTION
      /IWBEP/CX_MGW_BUSI_EXCEPTION .
  methods COMMON_INPUT_VALIDATION
    importing
      !IS_ER_ENTITY type TS_INPUT_PARAMETERS-ER_ENTITY
      !IS_AKTUELL_ARTIKEL type TS_INPUT_PARAMETERS-ER_ENTITY
    raising
      /IWBEP/CX_MGW_TECH_EXCEPTION
      /IWBEP/CX_MGW_BUSI_EXCEPTION .
  methods ASSIGN_INPUT_TO_DB_FORMAT
    importing
      !IS_CUD type CHAR1
      !IS_ER_ENTITY type TS_INPUT_PARAMETERS-ER_ENTITY
      !IS_ENTRY_TYPE type CHAR1
      !IS_AKTUELL_ARTIKEL type TS_INPUT_PARAMETERS-ER_ENTITY
    exporting
      !ES_T_MDARTIKEL type TT_T_MDARTIKEL
      !ES_T_MDARTTEXT type TT_T_MDARTTEXT
      !ES_ER_ENTITY type TS_INPUT_PARAMETERS-ER_ENTITY
    raising
      /IWBEP/CX_MGW_TECH_EXCEPTION
      /IWBEP/CX_MGW_BUSI_EXCEPTION .
  methods MAP_ENTITY_TO_DB_STRUC
    importing
      !IS_ER_ENTITY type TS_INPUT_PARAMETERS-ER_ENTITY
    exporting
      !ES_S_MDARTIKEL type TS_S_MDARTIKEL
      !ES_S_MDARTTEXT type TS_S_MDARTTEXT
    raising
      /IWBEP/CX_MGW_TECH_EXCEPTION
      /IWBEP/CX_MGW_BUSI_EXCEPTION .
  methods FILTER_DATA_BY_LANGU
    importing
      !IS_ET_ENTITY type TS_INPUT_PARAMETERS-ET_ENTITYSET
      !IS_LANGU type TS_S_MDARTTEXT-SPRACHE
    returning
      value(RV_ET_ENTITY) type TS_INPUT_PARAMETERS-ET_ENTITYSET
    raising
      /IWBEP/CX_MGW_TECH_EXCEPTION
      /IWBEP/CX_MGW_BUSI_EXCEPTION .
  methods SET_DESCR_BY_LANGU
    importing
      !IS_ET_ENTITY type TS_INPUT_PARAMETERS-ET_ENTITYSET
      !IS_ER_ENTITY type TS_INPUT_PARAMETERS-ER_ENTITY
      !IS_LANGU type TS_S_MDARTTEXT-SPRACHE
    returning
      value(RV_DESCRIPTION) type TS_S_MDARTTEXT-BESCHEIBUNG
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
  methods SET_EDITABLE_FLAG
    changing
      !CS_ET_ENTITY type TS_INPUT_PARAMETERS-ET_ENTITYSET
    raising
      /IWBEP/CX_MGW_TECH_EXCEPTION
      /IWBEP/CX_MGW_BUSI_EXCEPTION .
  methods SET_DEEPSTRUC_DETAILS
    importing
      !IS_ET_ENTITY type TS_INPUT_PARAMETERS-ET_ENTITYSET
    returning
      value(RV_ET_DEEPENTITY) type TT_DEEP_MDARTIKEL
    raising
      /IWBEP/CX_MGW_TECH_EXCEPTION
      /IWBEP/CX_MGW_BUSI_EXCEPTION .
  methods SET_KATALOGPREIS
    importing
      !IS_ER_ENTITY type TS_INPUT_PARAMETERS-ER_ENTITY
    returning
      value(RV_T_KATLOGPREIS) type ZCL_RS_3KATALOGPREIS=>TT_KATALOGPREIS
    raising
      /IWBEP/CX_MGW_TECH_EXCEPTION
      /IWBEP/CX_MGW_BUSI_EXCEPTION .
  methods SET_EDITABLE
    importing
      !IS_ER_ENTITY type TS_INPUT_PARAMETERS-ER_ENTITY
    returning
      value(RV_BOOLEAN) type TS_INPUT_PARAMETERS-ER_ENTITY-IS_EDITABLE
    raising
      /IWBEP/CX_MGW_TECH_EXCEPTION
      /IWBEP/CX_MGW_BUSI_EXCEPTION .
  methods SET_NOPREIS
    importing
      !IS_ER_ENTITY type TS_INPUT_PARAMETERS-ER_ENTITY
    returning
      value(RV_BOOLEAN) type TS_INPUT_PARAMETERS-ER_ENTITY-IS_NOPREIS
    raising
      /IWBEP/CX_MGW_TECH_EXCEPTION
      /IWBEP/CX_MGW_BUSI_EXCEPTION .
  methods SET_KOMPLEXITAET_DESC
    importing
      !IS_VALUE type STRING
    returning
      value(RV_DESCR) type STRING
    raising
      /IWBEP/CX_MGW_TECH_EXCEPTION
      /IWBEP/CX_MGW_BUSI_EXCEPTION .
  methods SET_KATEGORIE_DESC
    importing
      !IS_VALUE type STRING
    returning
      value(RV_DESCR) type STRING
    raising
      /IWBEP/CX_MGW_TECH_EXCEPTION
      /IWBEP/CX_MGW_BUSI_EXCEPTION .
  methods SET_LIEFEROBJEKT_DESC
    importing
      !IS_VALUE type STRING
    returning
      value(RV_DESCR) type STRING
    raising
      /IWBEP/CX_MGW_TECH_EXCEPTION
      /IWBEP/CX_MGW_BUSI_EXCEPTION .
ENDCLASS.



CLASS ZCL_RS_3MDARTIKEL IMPLEMENTATION.


  METHOD assign_input_to_db_format.
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
    REFRESH: es_t_mdartikel,
             es_t_mdarttext.
    DATA(ls_er_entity) = is_er_entity.
    DATA(ls_curr_er_entity) = is_aktuell_artikel.
    ls_er_entity-mandt = sy-mandt.
    es_er_entity = ls_er_entity.

    CASE is_cud.
      WHEN mc_cud_type-create.
        CASE is_entry_type.
          WHEN mc_entry_type-artikel.
            DATA(lv_langu) = sy-langu.
            DATA(lv_artikelnummer) = generate_artikel_nummer( ).
            ls_er_entity-sprache = lv_langu.
            ls_er_entity-artikelnummer = lv_artikelnummer.
            ls_er_entity-flag = abap_true.
            map_entity_to_db_struc( EXPORTING  is_er_entity   = ls_er_entity
                            IMPORTING  es_s_mdartikel = DATA(ls_s_mdartikel)
                                       es_s_mdarttext = DATA(ls_s_mdarttext) ).
            APPEND: ls_s_mdartikel TO es_t_mdartikel,
                    ls_s_mdarttext TO es_t_mdarttext.
          WHEN mc_entry_type-timeslice.
            ls_curr_er_entity-gueltig_bis = ls_er_entity-gueltig_ab - 1.
            ls_curr_er_entity-flag = abap_false.
            map_entity_to_db_struc( EXPORTING  is_er_entity   = ls_curr_er_entity
                                    IMPORTING  es_s_mdartikel = ls_s_mdartikel ).
            APPEND: ls_s_mdartikel TO es_t_mdartikel.


            ls_er_entity-flag = abap_true.
            map_entity_to_db_struc( EXPORTING  is_er_entity   = ls_er_entity
                                    IMPORTING  es_s_mdartikel = ls_s_mdartikel
                                               es_s_mdarttext = ls_s_mdarttext ).
            APPEND: ls_s_mdartikel TO es_t_mdartikel,
                    ls_s_mdarttext TO es_t_mdarttext.
          WHEN OTHERS.

        ENDCASE.
      WHEN mc_cud_type-update.
        map_entity_to_db_struc( EXPORTING  is_er_entity   = ls_er_entity
                                IMPORTING  es_s_mdartikel = ls_s_mdartikel
                                           es_s_mdarttext = ls_s_mdarttext ).
        APPEND: ls_s_mdartikel TO es_t_mdartikel,
                ls_s_mdarttext TO es_t_mdarttext.
      WHEN OTHERS.
    ENDCASE.

    es_er_entity = ls_er_entity.
  ENDMETHOD.


  METHOD common_input_validation.
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
    DATA(ls_er_entity) = is_er_entity.
    DATA(ls_current_entity) = is_aktuell_artikel.

    IF ls_er_entity-gueltig_ab IS INITIAL.
      MESSAGE i008(zrs_3) INTO DATA(lv_message).
      raise_error( is_message = lv_message ).
    ENDIF.
    IF ls_er_entity-gueltig_bis IS INITIAL.
      MESSAGE i009(zrs_3) INTO lv_message.
      raise_error( is_message = lv_message ).
    ENDIF.
    IF ls_er_entity-gueltig_ab > ls_er_entity-gueltig_bis.
      MESSAGE i010(zrs_3) INTO lv_message.
      raise_error( is_message = lv_message ).
    ENDIF.
    CONDENSE ls_er_entity-bescheibung.
    IF ls_er_entity-bescheibung IS INITIAL.
      MESSAGE i011(zrs_3) INTO lv_message.
      raise_error( is_message = lv_message ).
    ENDIF.
    IF ls_er_entity-nummer_lieferobjekt IS INITIAL.
      MESSAGE i012(zrs_3) INTO lv_message.
      raise_error( is_message = lv_message ).
    ENDIF.
    IF ls_er_entity-nummer_komplexitaet IS INITIAL.
      MESSAGE i013(zrs_3) INTO lv_message.
      raise_error( is_message = lv_message ).
    ENDIF.
    IF ls_er_entity-nummer_kategorie IS INITIAL.
      MESSAGE i014(zrs_3) INTO lv_message.
      raise_error( is_message = lv_message ).
    ENDIF.
    IF ls_er_entity-wert IS INITIAL.
      MESSAGE i015(zrs_3) INTO lv_message.
      raise_error( is_message = lv_message ).
    ENDIF.
    IF ls_er_entity-einheit_wert IS INITIAL.
      MESSAGE i016(zrs_3) INTO lv_message.
      raise_error( is_message = lv_message ).
    ENDIF.
    IF ls_er_entity-einzelvertrag IS INITIAL.
      MESSAGE i017(zrs_3) INTO lv_message.
      raise_error( is_message = lv_message ).
    ENDIF.
  ENDMETHOD.


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
    DATA(lo_katalog) = NEW zcl_rs_3mdartikel( is_params ).
    ro_entitat = lo_katalog.
  ENDMETHOD.


  METHOD define_artikel_details.
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
    DATA(ls_er_entity) = is_er_entity.
    DATA: ls_current_entity    TYPE ts_input_parameters-er_entity,
          lv_osql_where_clause TYPE string.

    IF is_er_entity IS INITIAL.
      RETURN.
    ENDIF.

    DATA(lv_langu) = ls_er_entity-sprache.

    CLEAR ls_current_entity.
    CASE is_cud.
      WHEN mc_cud_type-create.
        DATA(lv_filter_data) = mc_filter_values-to_create.
        REPLACE ALL OCCURRENCES: OF mc_filter_values-ampersand IN lv_filter_data WITH ls_er_entity-artikelnummer.

        lv_osql_where_clause = lv_filter_data.
        DATA(lt_current_entity) = get_db_values( lv_osql_where_clause ).
        set_editable_flag( CHANGING cs_et_entity = lt_current_entity ).
        DELETE lt_current_entity WHERE flag = abap_false.

        IF lines( lt_current_entity ) > 1.
          IF line_exists( lt_current_entity[ sprache = lv_langu ] ).
            ls_current_entity = lt_current_entity[ sprache = lv_langu ].
          ELSE.
            ls_current_entity = lt_current_entity[ 1 ].
          ENDIF.
        ELSEIF lines( lt_current_entity ) = 1.
          ls_current_entity = lt_current_entity[ 1 ].
        ENDIF.

        IF ls_current_entity IS NOT INITIAL.
          es_aktuell_artikel = ls_current_entity.
          es_entry_type = mc_entry_type-timeslice.
        ELSE.
          CLEAR es_aktuell_artikel.
          es_entry_type = mc_entry_type-artikel.
        ENDIF.
      WHEN mc_cud_type-update.
        lv_filter_data = mc_filter_values-to_update.
        REPLACE ALL OCCURRENCES: OF mc_filter_values-ampersand IN lv_filter_data WITH ls_er_entity-artikelnummer,
                                 OF mc_filter_values-asterisk IN lv_filter_data WITH ls_er_entity-gueltig_ab.

        lv_osql_where_clause = lv_filter_data.
        lt_current_entity = get_db_values( lv_osql_where_clause ).

        IF lines( lt_current_entity ) > 1.
          IF line_exists( lt_current_entity[ sprache = lv_langu ] ).
            ls_current_entity = lt_current_entity[ sprache = lv_langu ].
          ELSE.
            ls_current_entity = lt_current_entity[ 1 ].
          ENDIF.
        ELSEIF lines( lt_current_entity ) = 1.
          ls_current_entity = lt_current_entity[ 1 ].
        ENDIF.

        IF ls_current_entity IS NOT INITIAL.
          es_aktuell_artikel = ls_current_entity.
          es_entry_type = mc_entry_type-artikel.
        ELSE.
          MESSAGE i018(zrs_3) INTO DATA(lv_message).
          raise_error( is_message = lv_message ).
        ENDIF.
      WHEN OTHERS.
    ENDCASE.
  ENDMETHOD.


  METHOD filter_data_by_langu.
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
    DATA(lv_langu) = is_langu.
    rv_et_entity = lt_entity.
    IF lt_entity IS INITIAL.
      RETURN.
    ENDIF.
    DATA(lt_sub_entity) = lt_entity.
    SORT lt_sub_entity BY artikelnummer gueltig_ab.
    DELETE ADJACENT DUPLICATES FROM lt_sub_entity COMPARING artikelnummer gueltig_ab.
    IF lt_sub_entity IS INITIAL.
      RETURN.
    ENDIF.

    DATA(lt_consolidated_entity) = VALUE ts_input_parameters-et_entityset(
      FOR ls_entity IN lt_sub_entity
        (
            mandt = ls_entity-mandt
            artikelnummer = ls_entity-artikelnummer
            sprache = lv_langu
            gueltig_ab = ls_entity-gueltig_ab
            gueltig_bis = ls_entity-gueltig_bis
            bescheibung = set_descr_by_langu( is_et_entity   = lt_entity
                                              is_er_entity   = ls_entity
                                              is_langu       = lv_langu )

            nummer_lieferobjekt = ls_entity-nummer_lieferobjekt
            nummer_komplexitaet = ls_entity-nummer_komplexitaet
            nummer_kategorie = ls_entity-nummer_kategorie
            wert = ls_entity-wert
            einheit_wert = ls_entity-einheit_wert
            einzelvertrag = ls_entity-einzelvertrag
            notiz = ls_entity-notiz
            link = ls_entity-link
            flag = ls_entity-flag
            is_editable = set_editable( ls_entity )
            is_nopreis = set_nopreis( ls_entity )
            komplexitaet_desc = set_komplexitaet_desc( CONV string( ls_entity-nummer_komplexitaet ) )
            kategorie_desc = set_kategorie_desc( CONV string( ls_entity-nummer_kategorie ) )
            lieferobjekt_desc = set_lieferobjekt_desc( CONV string( ls_entity-nummer_lieferobjekt ) )
        )
       ).

    IF lt_consolidated_entity IS NOT INITIAL.
      REFRESH rv_et_entity.
      rv_et_entity = lt_consolidated_entity.
    ENDIF.
  ENDMETHOD.


  METHOD generate_artikel_nummer.
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
      MESSAGE i020(zrs_3) INTO lv_message.
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
* >001<   7.50       03.11.2020   DXC / Sigried Canezal
*         Fixes for Trichter-230
*----------------------------------------------------------------------*
    DATA(lv_osql_where_clause) = gs_params-io_tech_request_context_query->get_osql_where_clause( ).
    DATA(lv_langu) = sy-langu.
    CONDENSE lv_osql_where_clause.

    DATA(lv_mandt) = sy-mandt.
    DATA(lv_flag_isfilter) = abap_false.
    DATA(lv_flag_value) = abap_false.
    IF lv_osql_where_clause CS mc_filter_values-is_flag_trueformat.
      REPLACE ALL OCCURRENCES: OF mc_filter_values-is_flag_trueformat IN lv_osql_where_clause WITH mc_filter_values-mandt_filter,
                               OF mc_filter_values-ampersand IN lv_osql_where_clause WITH lv_mandt.
      lv_flag_isfilter = abap_true.
      lv_flag_value = abap_true.
    ELSEIF lv_osql_where_clause CS mc_filter_values-is_flag_falseformat.
      REPLACE ALL OCCURRENCES: OF mc_filter_values-is_flag_falseformat IN lv_osql_where_clause WITH mc_filter_values-mandt_filter,
                               OF mc_filter_values-ampersand IN lv_osql_where_clause WITH lv_mandt.
      lv_flag_isfilter = abap_true.
    ENDIF.

    DATA(lt_entity) = get_db_values( lv_osql_where_clause ).
    set_editable_flag( CHANGING cs_et_entity = lt_entity ).
    IF lv_flag_isfilter = abap_true.
      DELETE lt_entity WHERE flag <> lv_flag_value.
    ENDIF.

    lt_entity = filter_data_by_langu( is_et_entity = lt_entity
                                      is_langu     = lv_langu ).

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
            artikelnummer
            sprache
            gueltig_ab
            gueltig_bis
            bescheibung
            nummer_lieferobjekt
            nummer_komplexitaet
            nummer_kategorie
            wert
            einheit_wert
            einzelvertrag
            notiz
            link
    FROM zrs_d_3tt_mdartk
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
    CLEAR: es_s_mdartikel,
           es_s_mdarttext.
    IF is_er_entity IS INITIAL.
      RETURN.
    ENDIF.

    es_s_mdartikel = VALUE ts_s_mdartikel(
                              mandt = is_er_entity-mandt
                              artikelnummer = is_er_entity-artikelnummer
                              gueltig_ab = is_er_entity-gueltig_ab
                              gueltig_bis = is_er_entity-gueltig_bis
                              nummer_lieferobjekt = is_er_entity-nummer_lieferobjekt
                              nummer_komplexitaet = is_er_entity-nummer_komplexitaet
                              nummer_kategorie = is_er_entity-nummer_kategorie
                              wert = is_er_entity-wert
                              einheit_wert = is_er_entity-einheit_wert
                              einzelvertrag = is_er_entity-einzelvertrag
                              notiz = is_er_entity-notiz
                              link = is_er_entity-link
                            ).

    es_s_mdarttext = VALUE ts_s_mdarttext(
                                 mandt = is_er_entity-mandt
                                 artikelnummer = is_er_entity-artikelnummer
                                 sprache  = is_er_entity-sprache
                                 bescheibung = is_er_entity-bescheibung
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

    DATA(lt_sub_entity) = VALUE tt_deep_mdartikel( FOR ls_entity IN lt_entity
                           (
                            mandt	= ls_entity-mandt
                            artikelnummer	= ls_entity-artikelnummer
                            sprache	= ls_entity-sprache
                            gueltig_ab  = ls_entity-gueltig_ab
                            gueltig_bis	= ls_entity-gueltig_bis
                            bescheibung	= ls_entity-bescheibung
                            nummer_lieferobjekt	= ls_entity-nummer_lieferobjekt
                            nummer_komplexitaet	= ls_entity-nummer_komplexitaet
                            nummer_kategorie  = ls_entity-nummer_kategorie
                            wert  = ls_entity-wert
                            einheit_wert  = ls_entity-einheit_wert
                            einzelvertrag	= ls_entity-einzelvertrag
                            notiz	= ls_entity-notiz
                            link  = ls_entity-link
                            flag  = ls_entity-flag
                            is_editable	= ls_entity-is_editable
                            is_nopreis = set_nopreis( ls_entity )
                            komplexitaet_desc = set_komplexitaet_desc( CONV string( ls_entity-nummer_komplexitaet ) )
                            kategorie_desc = set_kategorie_desc( CONV string( ls_entity-nummer_kategorie ) )
                            lieferobjekt_desc = set_lieferobjekt_desc( CONV string( ls_entity-nummer_lieferobjekt ) )
                            tokatalogpreis = set_katalogpreis( ls_entity )
                         )
                        ).

    IF lt_sub_entity IS NOT INITIAL.
      REFRESH rv_et_deepentity.
      rv_et_deepentity = lt_sub_entity.
    ENDIF.
  ENDMETHOD.


  METHOD set_descr_by_langu.
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
    DATA(ls_entity) = is_er_entity.
    DATA(lv_langu) = is_langu.
    CLEAR rv_description.
    IF lt_entity IS INITIAL OR
       ls_entity IS INITIAL.
      RETURN.
    ENDIF.

    TRY.
        rv_description = lt_entity[ artikelnummer = ls_entity-artikelnummer
                                    gueltig_ab    = ls_entity-gueltig_ab
                                    sprache       = lv_langu ]-bescheibung.
      CATCH cx_sy_itab_line_not_found.
        CLEAR rv_description.
    ENDTRY.
  ENDMETHOD.


  METHOD set_editable.
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
    rv_boolean = abap_true.
    IF ls_entity IS INITIAL.
      RETURN.
    ENDIF.
    DATA ls_params TYPE zcl_rs_3abrufpos=>ts_input_parameters.
    DATA(lo_pos) = NEW zcl_rs_3abrufpos( ls_params ).
    DATA(lv_osql_where_clause) = mc_filter_values-to_update.
    REPLACE ALL OCCURRENCES: OF mc_filter_values-ampersand IN lv_osql_where_clause WITH ls_entity-artikelnummer,
                             OF mc_filter_values-asterisk IN lv_osql_where_clause WITH ls_entity-gueltig_ab.
    DATA(lt_pos) = lo_pos->get_db_values( lv_osql_where_clause ).
    IF line_exists( lt_pos[ abnahme_status = zif_rs_3constants=>gc_abrufpos_status_lp2-s900 ] ).
        DELETE lt_pos WHERE abnahme_status = zif_rs_3constants=>gc_abrufpos_status_lp2-s900.
    ENDIF.
    IF line_exists( lt_pos[ abnahme_status = zif_rs_3constants=>gc_abrufpos_status_lp2-s910 ] ).
        DELETE lt_pos WHERE abnahme_status = zif_rs_3constants=>gc_abrufpos_status_lp2-s910.
    ENDIF.

    IF lt_pos IS NOT INITIAL.
       rv_boolean = abap_false.
    ENDIF.
  ENDMETHOD.


  METHOD set_editable_flag.
    SORT cs_et_entity BY artikelnummer ASCENDING gueltig_ab DESCENDING.
    DATA(lt_unique_entity) = cs_et_entity.
    DELETE ADJACENT DUPLICATES FROM lt_unique_entity COMPARING artikelnummer.
    IF lt_unique_entity IS INITIAL.
      RETURN.
    ENDIF.

    DATA(lt_et_entity) = VALUE ts_input_parameters-et_entityset( FOR lw_entity IN cs_et_entity
                                     (
                                       mandt = lw_entity-mandt
                                       artikelnummer = lw_entity-artikelnummer
                                       sprache = lw_entity-sprache
                                       gueltig_ab = lw_entity-gueltig_ab
                                       gueltig_bis = lw_entity-gueltig_bis
                                       bescheibung = lw_entity-bescheibung
                                       nummer_lieferobjekt = lw_entity-nummer_lieferobjekt
                                       nummer_komplexitaet = lw_entity-nummer_komplexitaet
                                       nummer_kategorie = lw_entity-nummer_kategorie
                                       wert = lw_entity-wert
                                       einheit_wert = lw_entity-einheit_wert
                                       einzelvertrag = lw_entity-einzelvertrag
                                       notiz = lw_entity-notiz
                                       link = lw_entity-link
                                       flag = COND char01( WHEN line_exists( lt_unique_entity[ artikelnummer = lw_entity-artikelnummer
                                                                                              gueltig_ab = lw_entity-gueltig_ab ] )
                                                           THEN abap_true
                                                           ELSE abap_false )
                                     )
                                 ).
    IF lt_et_entity IS NOT INITIAL.
      REFRESH cs_et_entity.
      cs_et_entity = lt_et_entity.
    ENDIF.
  ENDMETHOD.


  METHOD set_katalogpreis.
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
    REFRESH rv_t_katlogpreis.
    IF ls_entity IS INITIAL.
      RETURN.
    ENDIF.
    DATA ls_params TYPE zcl_rs_3katalogpreis=>ts_input_parameters.
    DATA(lo_preis) = NEW zcl_rs_3katalogpreis( ls_params ).
    DATA(lv_osql_where_clause) = mc_filter_values-to_katalogpreis.
    REPLACE ALL OCCURRENCES: OF mc_filter_values-ampersand IN lv_osql_where_clause WITH ls_entity-einheit_wert,
                             OF mc_filter_values-at_sign IN lv_osql_where_clause WITH ls_entity-gueltig_ab.
    DATA(lt_preis) = lo_preis->get_db_values( lv_osql_where_clause ).
    SORT lt_preis BY gueltig_bis DESCENDING.
    IF lt_preis IS NOT INITIAL.
      APPEND LINES OF lt_preis TO rv_t_katlogpreis.
    ENDIF.
  ENDMETHOD.


  METHOD set_kategorie_desc.
    DATA(lv_value) = is_value.
    CLEAR rv_descr.
    IF lv_value IS INITIAL.
      RETURN.
    ENDIF.

    DATA ls_params TYPE zcl_rs_3katalogkategorie=>ts_input_parameters.
    DATA(lv_osql_where_clause) = zcl_rs_3katalogkategorie=>mc_filter_values-to_read.
    REPLACE ALL OCCURRENCES OF zcl_rs_3katalogkategorie=>mc_filter_values-ampersand IN lv_osql_where_clause WITH lv_value.
    DATA(lt_db_values) = NEW zcl_rs_3katalogkategorie( ls_params )->get_db_values( lv_osql_where_clause ).

    IF lt_db_values IS NOT INITIAL.
      rv_descr = lt_db_values[ 1 ]-bezeichnung.
    ENDIF.
  ENDMETHOD.


  METHOD set_komplexitaet_desc.
    DATA(lv_value) = is_value.
    CLEAR rv_descr.
    IF lv_value IS INITIAL.
      RETURN.
    ENDIF.

    DATA ls_params TYPE zcl_rs_3katalogkomplexitat=>ts_input_parameters.
    DATA(lv_osql_where_clause) = zcl_rs_3katalogkomplexitat=>mc_filter_values-to_read.
    REPLACE ALL OCCURRENCES OF zcl_rs_3katalogkomplexitat=>mc_filter_values-ampersand IN lv_osql_where_clause WITH lv_value.
    DATA(lt_db_values) = NEW zcl_rs_3katalogkomplexitat( ls_params )->get_db_values( lv_osql_where_clause ).

    IF lt_db_values IS NOT INITIAL.
      rv_descr = lt_db_values[ 1 ]-beschreibung.
    ENDIF.
  ENDMETHOD.


  METHOD set_lieferobjekt_desc.
    DATA(lv_value) = is_value.
    CLEAR rv_descr.
    IF lv_value IS INITIAL.
      RETURN.
    ENDIF.

    DATA ls_params TYPE zcl_rs_3kataloglieferobjekte=>ts_input_parameters.
    DATA(lv_osql_where_clause) = zcl_rs_3kataloglieferobjekte=>mc_filter_values-to_read.
    REPLACE ALL OCCURRENCES OF zcl_rs_3kataloglieferobjekte=>mc_filter_values-ampersand IN lv_osql_where_clause WITH lv_value.
    DATA(lt_db_values) = NEW zcl_rs_3kataloglieferobjekte( ls_params )->get_db_values( lv_osql_where_clause ).

    IF lt_db_values IS NOT INITIAL.
       rv_descr = lt_db_values[ 1 ]-beschreibung.
    ENDIF.
  ENDMETHOD.


  METHOD SET_NOPREIS.
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
    rv_boolean = abap_true.
    IF ls_entity IS INITIAL.
      RETURN.
    ENDIF.
    DATA ls_params TYPE zcl_rs_3katalogpreis=>ts_input_parameters.
    DATA(lo_preis) = NEW zcl_rs_3katalogpreis( ls_params ).
    DATA(lv_osql_where_clause) = mc_filter_values-to_katalogpreis.
    REPLACE ALL OCCURRENCES: OF mc_filter_values-ampersand IN lv_osql_where_clause WITH ls_entity-einheit_wert,
                             OF mc_filter_values-at_sign IN lv_osql_where_clause WITH ls_entity-gueltig_ab.
    DATA(lt_preis) = lo_preis->get_db_values( lv_osql_where_clause ).
    IF LINES( lt_preis ) > 1.
       rv_boolean = abap_true.
    ELSEIF LINES( lt_preis ) = 1.
       rv_boolean = abap_false.
    ELSE.
       rv_boolean = abap_true.
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
    DATA(lt_mdartikel) = is_t_mdartikel.
    DATA(lt_mdarttext) = is_t_mdarttext.
    DATA lv_message TYPE string.

    IF lines( lt_mdartikel ) >= 1.
      "insert value/update data
      MODIFY zrs_3tt_mdartikl FROM TABLE lt_mdartikel.
      CASE sy-subrc.
        WHEN 0.
          COMMIT WORK.
        WHEN OTHERS.
          MESSAGE i006(zrs_3) INTO lv_message.
          raise_error( is_message = lv_message ).
      ENDCASE.
    ENDIF.

    IF lines( lt_mdarttext ) >= 1.
      MODIFY zrs_3tt_mdartikt FROM TABLE lt_mdarttext.
      CASE sy-subrc.
        WHEN 0.
          COMMIT WORK.
        WHEN OTHERS.
          MESSAGE i007(zrs_3) INTO lv_message.
          raise_error( is_message = lv_message ).
      ENDCASE.
    ENDIF.
  ENDMETHOD.


  METHOD validate_input_values.
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
    DATA(ls_er_entity) = is_er_entity.
    DATA(ls_current_entity) = is_aktuell_artikel.
    CASE is_cud.
      WHEN mc_cud_type-create.
        CASE is_entry_type.
          WHEN mc_entry_type-artikel.
            common_input_validation( is_er_entity = ls_er_entity
                                     is_aktuell_artikel = ls_current_entity ).

            DATA(lv_serviceobj) = mc_filter_values-to_serviceobj.
            REPLACE ALL OCCURRENCES OF mc_filter_values-ampersand IN lv_serviceobj WITH ls_er_entity-nummer_lieferobjekt.

            DATA(lv_complexity) = mc_filter_values-to_complexity.
            REPLACE ALL OCCURRENCES OF mc_filter_values-ampersand IN lv_complexity WITH ls_er_entity-nummer_komplexitaet.

            DATA(lv_category) = mc_filter_values-to_category.
            REPLACE ALL OCCURRENCES OF mc_filter_values-ampersand IN lv_category WITH ls_er_entity-nummer_kategorie.

            DATA(lv_osql_where_clause) = |{ lv_serviceobj } { mc_filter_values-and } { lv_complexity } { mc_filter_values-and } { lv_category }|.
            DATA(lt_current_entity) = get_db_values( lv_osql_where_clause ).
            IF lt_current_entity IS NOT INITIAL.
              MESSAGE i019(zrs_3) INTO DATA(lv_message).
              raise_error( is_message = lv_message ).
            ENDIF.
          WHEN mc_entry_type-timeslice.
            IF ls_er_entity-artikelnummer IS INITIAL.
              MESSAGE i020(zrs_3) INTO lv_message.
              raise_error( is_message = lv_message ).
            ENDIF.
            IF ls_er_entity-artikelnummer <> ls_current_entity-artikelnummer.
              MESSAGE i021(zrs_3) INTO lv_message.
              raise_error( is_message = lv_message ).
            ENDIF.
            common_input_validation( is_er_entity = ls_er_entity
                                     is_aktuell_artikel = ls_current_entity ).

            IF ls_er_entity-gueltig_ab <= ls_current_entity-gueltig_ab.
              MESSAGE i022(zrs_3) INTO lv_message.
              raise_error( is_message = lv_message ).
            ENDIF.

            IF ls_er_entity-nummer_lieferobjekt <> ls_current_entity-nummer_lieferobjekt.
              MESSAGE i023(zrs_3) INTO lv_message.
              raise_error( is_message = lv_message ).
            ENDIF.
            IF ls_er_entity-nummer_komplexitaet <> ls_current_entity-nummer_komplexitaet.
              MESSAGE i024(zrs_3) INTO lv_message.
              raise_error( is_message = lv_message ).
            ENDIF.
            IF ls_er_entity-nummer_kategorie <> ls_current_entity-nummer_kategorie.
              MESSAGE i025(zrs_3) INTO lv_message.
              raise_error( is_message = lv_message ).
            ENDIF.
          WHEN OTHERS.
        ENDCASE.
      WHEN mc_cud_type-update.
        IF ls_er_entity-artikelnummer IS INITIAL.
          MESSAGE i020(zrs_3) INTO lv_message.
          raise_error( is_message = lv_message ).
        ENDIF.
        common_input_validation( is_er_entity = ls_er_entity
                                 is_aktuell_artikel = ls_current_entity ).

        IF ls_er_entity-nummer_lieferobjekt <> ls_current_entity-nummer_lieferobjekt.
          MESSAGE i023(zrs_3) INTO lv_message.
          raise_error( is_message = lv_message ).
        ENDIF.
        IF ls_er_entity-nummer_komplexitaet <> ls_current_entity-nummer_komplexitaet.
          MESSAGE i024(zrs_3) INTO lv_message.
          raise_error( is_message = lv_message ).
        ENDIF.
        IF ls_er_entity-nummer_kategorie <> ls_current_entity-nummer_kategorie.
          MESSAGE i025(zrs_3) INTO lv_message.
          raise_error( is_message = lv_message ).
        ENDIF.
        IF ls_er_entity-gueltig_ab <> ls_current_entity-gueltig_ab.
          MESSAGE i026(zrs_3) INTO lv_message.
          raise_error( is_message = lv_message ).
        ENDIF.
        IF ls_er_entity-wert <> ls_current_entity-wert.
          MESSAGE i027(zrs_3) INTO lv_message.
          raise_error( is_message = lv_message ).
        ENDIF.
        IF ls_er_entity-einheit_wert <> ls_current_entity-einheit_wert.
          MESSAGE i028(zrs_3) INTO lv_message.
          raise_error( is_message = lv_message ).
        ENDIF.
      WHEN OTHERS.
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
    DATA ls_entity LIKE gs_params-er_entity.
    gs_params-io_data_provider->read_entry_data( IMPORTING es_data = ls_entity ).
    gs_params-er_entity = ls_entity.
    IF ls_entity IS INITIAL.
      RETURN.
    ENDIF.

*    define artikel
    define_artikel_details( EXPORTING   is_cud  = mc_cud_type-create
                                        is_er_entity  = ls_entity
                            IMPORTING   es_entry_type      = DATA(lv_entry_type)
                                        es_aktuell_artikel = DATA(ls_curr_artikel) ).

*    validate input fields
    validate_input_values( is_cud             = mc_cud_type-create
                           is_er_entity       = ls_entity
                           is_entry_type      = lv_entry_type
                           is_aktuell_artikel = ls_curr_artikel ).

*       Assign or map data to structure/table format
    assign_input_to_db_format( EXPORTING   is_cud             = mc_cud_type-create
                                           is_er_entity       = ls_entity
                                           is_entry_type      = lv_entry_type
                                           is_aktuell_artikel = ls_curr_artikel
                               IMPORTING   es_t_mdartikel     = DATA(lt_mdartikel)
                                           es_t_mdarttext    = DATA(lt_mdarttext)
                                           es_er_entity      = gs_params-er_entity ).

*   submit data to db
    submit_db_values(  is_t_mdartikel = lt_mdartikel
                       is_t_mdarttext = lt_mdarttext ).

    DATA(lo_response) = NEW zcl_rs_3mdartikel( gs_params ).
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
    DATA(lo_response) = NEW zcl_rs_3mdartikel( gs_params ).
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

    DATA(lv_osql_where_clause) = gs_params-io_tech_request_context_query->get_osql_where_clause( ).
    DATA(lv_langu) = sy-langu.
    CONDENSE lv_osql_where_clause.

    DATA(lv_mandt) = sy-mandt.
    DATA(lv_flag_isfilter) = abap_false.
    DATA(lv_flag_value) = abap_false.
    IF lv_osql_where_clause CS mc_filter_values-is_flag_trueformat.
      REPLACE ALL OCCURRENCES: OF mc_filter_values-is_flag_trueformat IN lv_osql_where_clause WITH mc_filter_values-mandt_filter,
                               OF mc_filter_values-ampersand IN lv_osql_where_clause WITH lv_mandt.
      lv_flag_isfilter = abap_true.
      lv_flag_value = abap_true.
    ELSEIF lv_osql_where_clause CS mc_filter_values-is_flag_falseformat.
      REPLACE ALL OCCURRENCES: OF mc_filter_values-is_flag_falseformat IN lv_osql_where_clause WITH mc_filter_values-mandt_filter,
                               OF mc_filter_values-ampersand IN lv_osql_where_clause WITH lv_mandt.
      lv_flag_isfilter = abap_true.
    ENDIF.

    APPEND gs_params-io_tech_request_context_query->get_expand( ) TO gs_params-et_expanded_tech_clauses.

    DATA(lt_entity) = get_db_values( lv_osql_where_clause ).
    set_editable_flag( CHANGING cs_et_entity = lt_entity ).
    IF lv_flag_isfilter = abap_true.
      DELETE lt_entity WHERE flag <> lv_flag_value.
    ENDIF.

    lt_entity = filter_data_by_langu( is_et_entity = lt_entity
                                      is_langu     = lv_langu ).

    DATA(lt_deep_entity) = set_deepstruc_details( lt_entity ).

    IF lt_deep_entity  IS NOT INITIAL.
      copy_data_to_ref(
         EXPORTING
          is_data = lt_deep_entity
         CHANGING
          cs_data = gs_params-er_entityset_expand ).
    ENDIF.

    DATA(lo_response) = NEW zcl_rs_3mdartikel( gs_params ).
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
    IF NOT line_exists( lt_keys[ name = mc_key_field ] ) OR lt_keys IS INITIAL.
      RETURN.
    ENDIF.

    DATA(lv_artikelnummer) = lt_keys[ name = mc_key_field ]-value.
    DATA(lv_langu) = sy-langu.
    DATA(lv_sprache_filter) = mc_filter_values-sprache_filter.
    REPLACE ALL OCCURRENCES: OF mc_filter_values-ampersand IN lv_sprache_filter WITH lv_langu,
                             OF mc_filter_values-ampersand IN lv_osql_where_clause WITH lv_artikelnummer.
    CONDENSE lv_osql_where_clause.

    lv_osql_where_clause = |{ lv_osql_where_clause } { mc_filter_values-and } { lv_sprache_filter }|.

    DATA(lt_entity) = get_db_values( lv_osql_where_clause ).
    set_editable_flag( CHANGING cs_et_entity = lt_entity ).
    lt_entity = filter_data_by_langu( is_et_entity = lt_entity
                                      is_langu     = lv_langu ).

    IF lt_entity IS NOT INITIAL.
      gs_params-er_entity = lt_entity[ 1 ].
    ENDIF.

    DATA(lo_response) = NEW zcl_rs_3mdartikel( gs_params ).
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
    DATA ls_entity LIKE gs_params-er_entity.
    gs_params-io_data_provider->read_entry_data( IMPORTING es_data = ls_entity ).
    gs_params-er_entity = ls_entity.
    IF ls_entity IS INITIAL.
      RETURN.
    ENDIF.

*    define artikel
    define_artikel_details( EXPORTING   is_cud  = mc_cud_type-update
                                        is_er_entity  = ls_entity
                            IMPORTING   es_entry_type      = DATA(lv_entry_type)
                                        es_aktuell_artikel = DATA(ls_curr_artikel) ).

*    validate input fields
    validate_input_values( is_cud             = mc_cud_type-update
                           is_er_entity       = ls_entity
                           is_entry_type      = lv_entry_type
                           is_aktuell_artikel = ls_curr_artikel ).

*       Assign or map data to structure/table format
    assign_input_to_db_format( EXPORTING    is_cud             = mc_cud_type-update
                                            is_er_entity       = ls_entity
                                            is_entry_type      = lv_entry_type
                                            is_aktuell_artikel = ls_curr_artikel
                                IMPORTING   es_t_mdartikel     = DATA(lt_mdartikel)
                                            es_t_mdarttext    = DATA(lt_mdarttext)
                                            es_er_entity      = gs_params-er_entity ).

*   submit data to db
    submit_db_values(  is_t_mdartikel = lt_mdartikel
                       is_t_mdarttext = lt_mdarttext ).

    DATA(lo_response) = NEW zcl_rs_3mdartikel( gs_params ).
    rv_if_response = lo_response.
  ENDMETHOD.
ENDCLASS.
