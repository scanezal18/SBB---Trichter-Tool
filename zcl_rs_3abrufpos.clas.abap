class ZCL_RS_3ABRUFPOS definition
  public
  final
  create public

  global friends ZIF_RS_3DPC_ABRUFSTATUS
                 ZIF_RS_3DPC_ANTWORT
                 ZIF_RS_3DPC_ENTITAT .

public section.

  interfaces ZIF_RS_3DPC_ANTWORT .
  interfaces ZIF_RS_3DPC_ENTITAT .

  types TS_ABRUFPOS type ZRS_3TT_ABRUFPOS_S .
  types:
    tt_abrufpos TYPE STANDARD TABLE OF ts_abrufpos WITH DEFAULT KEY .
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
        er_entity                      TYPE ts_abrufpos,
        et_entityset                   TYPE tt_abrufpos,
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

  data:
    BEGIN OF gs_deep_abrufpos.
            INCLUDE TYPE ts_abrufpos.
            DATA: tostatusset TYPE zcl_rs_3prjabrstat=>tt_prjabrst,
          END OF gs_deep_abrufpos .

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

  types TS_S_ABRUFPOS type ZRS_3TT_ABRUFPOS .
  types:
    tt_t_abrufpos TYPE STANDARD TABLE OF ts_s_abrufpos WITH DEFAULT KEY .
  types:
    tt_deep_abrufpos LIKE STANDARD TABLE OF gs_deep_abrufpos WITH DEFAULT KEY .
  types TS_S_KATLIEFT type ZRS_3TT_KATLIEFT .
  types:
    tt_t_katlieft TYPE STANDARD TABLE OF ts_s_katlieft WITH DEFAULT KEY .

  data GS_PARAMS type TS_INPUT_PARAMETERS .
  constants MC_VERBINDER type CHAR2 value '=>' ##NO_TEXT.
  constants:
    BEGIN OF mc_filter_values,
        status_filter          TYPE string VALUE '( Status eq `&` )' ##NO_TEXT,
        sprache_filter         TYPE string VALUE '( Sprache eq `&` )' ##NO_TEXT,
        mandt_filter           TYPE string VALUE '( Mandt eq `&` )' ##NO_TEXT,
        vorhabensnummer_filter TYPE string VALUE '( Vorhabensnummer eq `&` )' ##NO_TEXT,
        and                    TYPE string VALUE 'and' ##NO_TEXT,
        or                     TYPE string VALUE 'or' ##NO_TEXT,
        at_sign                TYPE string VALUE '@' ##NO_TEXT,
        ampersand              TYPE string VALUE '&' ##NO_TEXT,
        asterisk               TYPE string VALUE '*' ##NO_TEXT,
        is_edit_value          TYPE string VALUE 'IS_EDIT',
        is_edit_trueformat     TYPE string VALUE `( IS_EDIT = 'X' )`,
        to_read_abrufkopf      TYPE string VALUE '( Abrufnummer eq `&` and Status eq `@` )' ##NO_TEXT,
        to_read_keys           TYPE string VALUE '( Abrufnummer eq `&` and Abrufposition eq `@` )' ##NO_TEXT,
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
  constants MC_KEY_SEC_FIELD type STRING value 'ABRUFPOSITION' ##NO_TEXT.

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
  methods SUBMIT_DB_VALUES
    importing
      !IS_T_ABRUFPOS type TT_T_ABRUFPOS
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
      value(RV_ER_ENTITY) type TS_S_ABRUFPOS
    raising
      /IWBEP/CX_MGW_TECH_EXCEPTION
      /IWBEP/CX_MGW_BUSI_EXCEPTION .
  methods GET_STATUSET
    importing
      !IS_ABRUFNUMMER type TS_ABRUFPOS-ABRUFNUMMER
      !IS_STATUS type TS_ABRUFPOS-ABNAHME_STATUS
    returning
      value(RV_T_STATUS) type ZCL_RS_3PRJABRSTAT=>TT_PRJABRST
    raising
      /IWBEP/CX_MGW_TECH_EXCEPTION
      /IWBEP/CX_MGW_BUSI_EXCEPTION .
  methods SET_DEEPSTRUC_DETAILS
    importing
      !IS_ET_ENTITY type TS_INPUT_PARAMETERS-ET_ENTITYSET
    returning
      value(RV_ET_DEEPENTITY) type TT_DEEP_ABRUFPOS
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
  methods SET_STATUS_DESCRIPTIONS
    importing
      !IS_ET_ENTITY type TS_INPUT_PARAMETERS-ET_ENTITYSET
    returning
      value(RV_ET_ENTITY) type TS_INPUT_PARAMETERS-ET_ENTITYSET
    raising
      /IWBEP/CX_MGW_TECH_EXCEPTION
      /IWBEP/CX_MGW_BUSI_EXCEPTION .
  methods HANDLE_ABRUFKOPF
    importing
      !IS_ER_ENTITY type ZCL_RS_3ABRUFKOPF=>TS_INPUT_PARAMETERS-ER_ENTITY
    raising
      /IWBEP/CX_MGW_TECH_EXCEPTION
      /IWBEP/CX_MGW_BUSI_EXCEPTION .
  methods HANDLE_VERRCHNG
    importing
      !IS_ER_ENTITY type ZCL_RS_3VERRCHNG=>TS_INPUT_PARAMETERS-ER_ENTITY
    raising
      /IWBEP/CX_MGW_TECH_EXCEPTION
      /IWBEP/CX_MGW_BUSI_EXCEPTION .
  methods HANDLE_EMAIL
    importing
      !IS_EMAIL_PARAMS type ZIF_RS_3DPC_ABRUFSTATUS=>TS_EMAIL_PARAMS
    returning
      value(RV_EMAIL_PARAMS) type ZIF_RS_3DPC_ABRUFSTATUS=>TS_EMAIL_PARAMS
    raising
      /IWBEP/CX_MGW_TECH_EXCEPTION
      /IWBEP/CX_MGW_BUSI_EXCEPTION .
  methods GET_KATLIEF_ANTEIL
    importing
      !IS_LIEFERANTEIL type TS_S_KATLIEFT-LIEFERANTEIL
    returning
      value(RV_ANTEIL) type TS_S_KATLIEFT-ANTEIL
    raising
      /IWBEP/CX_MGW_TECH_EXCEPTION
      /IWBEP/CX_MGW_BUSI_EXCEPTION .
  methods HANDLE_ABRUFPOS
    importing
      !IS_ER_ENTITY type TS_INPUT_PARAMETERS-ER_ENTITY
    returning
      value(RV_ER_ENTITY) type TS_INPUT_PARAMETERS-ER_ENTITY
    raising
      /IWBEP/CX_MGW_TECH_EXCEPTION
      /IWBEP/CX_MGW_BUSI_EXCEPTION .
ENDCLASS.



CLASS ZCL_RS_3ABRUFPOS IMPLEMENTATION.


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
    DATA(lo_abruf) = NEW zcl_rs_3abrufpos( is_params ).
    ro_entitat = lo_abruf.
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
            abrufposition
            artikelnummer
            gueltig_ab
            referenz
            wert_objekt
            einheit_wert
            menge
            wert_position
            kosten_position
            waehrung
            geplantes_lieferdatum
            realisiertes_lieferdatum
            abgerechnet
            storniert
            datum_storno
            abrufer
            abrufdatum
            rueckruf_datum
            rueckruf_grund
            abnahme_status
            notiz
            geloescht
            attachment
            lastchangedby
            lastchangedon
    FROM zrs_d_3tt_abrufp
    INTO TABLE rv_et_entity
    WHERE (lv_osql_where_clause).
    IF rv_et_entity IS NOT INITIAL.
      RETURN.
    ENDIF.
  ENDMETHOD.


  METHOD get_katlief_anteil.
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
    TYPES: BEGIN OF ts_katlief_antiel,
             anteil         TYPE ts_s_katlieft-anteil,
             einheit_anteil TYPE ts_s_katlieft-einheit_anteil,
           END OF ts_katlief_antiel.
    DATA ls_katlief_antiel TYPE ts_katlief_antiel.
    DATA(lv_lieferanteil) = is_lieferanteil.
    CLEAR rv_anteil.
    IF lv_lieferanteil IS INITIAL.
      RETURN.
    ENDIF.

    SELECT SINGLE anteil
                  einheit_anteil
      FROM zrs_3tt_katlieft
      INTO ls_katlief_antiel
     WHERE lieferanteil = lv_lieferanteil.
    IF sy-subrc = 0.
      CASE ls_katlief_antiel-einheit_anteil.
        WHEN zif_rs_3constants=>gc_einheit_antiel-percentage."percentage
          rv_anteil = ls_katlief_antiel-anteil / 100.
        WHEN OTHERS.
          rv_anteil = ls_katlief_antiel-anteil.
      ENDCASE.
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
    REFRESH: rv_t_status.
    DATA ls_input TYPE zcl_rs_3prjabrstat=>ts_input_parameters.
    DATA(lv_input) = is_status.
    DATA(lv_abrufnummer) = is_abrufnummer.
    IF lv_input IS INITIAL OR
       lv_abrufnummer IS INITIAL.
      RETURN.
    ENDIF.
    DATA(lo_status) = NEW zcl_rs_3prjabrstat( ls_input ).
    DATA(lv_osql_where_clause) = lo_status->mc_filter_values-to_read_stat.
    DATA(lv_langu) = sy-langu.
    DATA lt_status_range LIKE zcl_rs_3prjabrstat=>gt_status.

    DATA ls_input_params TYPE zcl_rs_3abrufkopf=>ts_input_parameters.
    DATA(lo_abrufkopf) = NEW zcl_rs_3abrufkopf( ls_input_params ).


    DATA(lv_abruf_filter) = lo_abrufkopf->mc_filter_values-abrufnummer_filter.
    REPLACE ALL OCCURRENCES: OF lo_abrufkopf->mc_filter_values-ampersand IN lv_abruf_filter WITH lv_abrufnummer.
    CONDENSE lv_abruf_filter.
    DATA(lt_abrufkopf_entity) = lo_abrufkopf->get_db_values( lv_abruf_filter ).

    IF lt_abrufkopf_entity IS NOT INITIAL.
      DATA(ls_abrufkopf) = lt_abrufkopf_entity[ 1 ].
      CASE ls_abrufkopf-einzelvertrag.
          WHEN zif_rs_3constants=>gc_abruf_contract_lp2.
          DATA(lv_procezzart) = zif_rs_3constants=>gc_procezzart_type_abrufpos-lp2.
          WHEN zif_rs_3constants=>gc_abruf_contract_lp3.
          lv_procezzart = zif_rs_3constants=>gc_procezzart_type_abrufpos-lp3.
        WHEN zif_rs_3constants=>gc_abruf_contract_lp5.
          lv_procezzart = zif_rs_3constants=>gc_procezzart_type_abrufpos-lp2.
          WHEN OTHERS.
          RETURN.
      ENDCASE.
    ELSE.
      RETURN.
    ENDIF.

    REPLACE ALL OCCURRENCES: OF lo_status->mc_filter_values-ampersand IN lv_osql_where_clause WITH lv_langu,
                             OF lo_status->mc_filter_values-at_sign IN lv_osql_where_clause WITH lv_procezzart,
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
       REFRESH:  lo_status->gt_status.
      lo_status->gt_status = lt_status_range.

      CLEAR lv_osql_where_clause.
      lv_osql_where_clause = lo_status->mc_filter_values-to_read_multstat.
      REPLACE ALL OCCURRENCES: OF lo_status->mc_filter_values-ampersand IN lv_osql_where_clause WITH lv_langu,
                               OF lo_status->mc_filter_values-at_sign IN lv_osql_where_clause WITH lv_procezzart.
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


  METHOD handle_abrufkopf.
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
    DATA(ls_entity_kopf) = is_er_entity.

    IF ls_entity_kopf IS INITIAL.
      RETURN.
    ENDIF.

    DATA: ls_kopf_params TYPE zcl_rs_3abrufkopf=>ts_input_parameters,
          lt_entity_kopf TYPE zcl_rs_3abrufkopf=>tt_t_abrufkopf.

    DATA(lo_abrufkopf) = NEW zcl_rs_3abrufkopf( ls_kopf_params ).
    APPEND lo_abrufkopf->map_entity_to_db_struc( ls_entity_kopf ) TO lt_entity_kopf.
    lo_abrufkopf->submit_db_values( lt_entity_kopf ).
  ENDMETHOD.


  METHOD HANDLE_ABRUFPOS.
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
    DATA(ls_entity_pos) = is_er_entity.
    DATA lt_entity TYPE tt_t_abrufpos.
    rv_er_entity = ls_entity_pos.

    IF ls_entity_pos IS INITIAL.
      RETURN.
    ENDIF.

    APPEND map_entity_to_db_struc( ls_entity_pos ) TO lt_entity.
    submit_db_values( lt_entity ).

    CLEAR rv_er_entity.
    rv_er_entity = ls_entity_pos.
  ENDMETHOD.


  METHOD HANDLE_EMAIL.
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
    DATA(ls_email_params) = is_email_params.
    rv_email_params = ls_email_params.
    IF ls_email_params IS INITIAL.
       RETURN.
    ENDIF.

    NEW zcl_rs_3vorhaben( )->send_email(
        EXPORTING
          iv_mail_id             = ls_email_params-iv_mail_id
          iv_sender_address      = ls_email_params-iv_sender_address
          iv_sender_address_type = ls_email_params-iv_sender_address_type
          iv_language            = ls_email_params-iv_language
          iv_send_immediately    = ls_email_params-iv_send_immediately
          iv_html                = ls_email_params-iv_html
          iv_subject_as_title    = ls_email_params-iv_subject_as_title
          it_name_value          = ls_email_params-it_name_value
          it_recipients          = ls_email_params-it_recipients
         IMPORTING
          ev_error               = ls_email_params-ev_error
        CHANGING
          ct_messages            = ls_email_params-ct_messages
          ).

    rv_email_params = ls_email_params.
  ENDMETHOD.


  METHOD handle_verrchng.
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
    DATA(ls_entity_billing) = is_er_entity.

    IF ls_entity_billing IS INITIAL.
      RETURN.
    ENDIF.

    DATA: ls_verrchng_params TYPE zcl_rs_3verrchng=>ts_input_parameters,
          lt_entity_verrchng TYPE zcl_rs_3verrchng=>tt_t_verrchng.

    DATA(lo_verrchng) = NEW zcl_rs_3verrchng( ls_verrchng_params ).
    APPEND lo_verrchng->map_entity_to_db_struc( ls_entity_billing ) TO lt_entity_verrchng.
    lo_verrchng->submit_db_values( lt_entity_verrchng ).
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
    DATA(ls_entity) = is_er_entity.
    IF ls_entity IS INITIAL.
      RETURN.
    ENDIF.

    rv_er_entity = VALUE ts_s_abrufpos(
                                    mandt = ls_entity-mandt
                                    abrufnummer = ls_entity-abrufnummer
                                    abrufposition	= ls_entity-abrufposition
                                    artikelnummer	= ls_entity-artikelnummer
                                    gueltig_ab  = ls_entity-gueltig_ab
                                    referenz  = ls_entity-referenz
                                    wert_objekt	= ls_entity-wert_objekt
                                    einheit_wert  = ls_entity-einheit_wert
                                    menge	= ls_entity-menge
                                    wert_position	= ls_entity-wert_position
                                    kosten_position	= ls_entity-kosten_position
                                    waehrung  = ls_entity-waehrung
                                    geplantes_lieferdatum	= ls_entity-geplantes_lieferdatum
                                    realisiertes_lieferdatum  = ls_entity-realisiertes_lieferdatum
                                    abgerechnet	= ls_entity-abgerechnet
                                    storniert	= ls_entity-storniert
                                    datum_storno  = ls_entity-datum_storno
                                    abrufer	= ls_entity-abrufer
                                    abrufdatum  = ls_entity-abrufdatum
                                    rueckruf_datum  = ls_entity-rueckruf_datum
                                    rueckruf_grund  = ls_entity-rueckruf_grund
                                    abnahme_status  = ls_entity-abnahme_status
                                    notiz	= ls_entity-notiz
                                    geloescht	= ls_entity-geloescht
                                    attachment  = ls_entity-attachment
                                    lastchangedby	= ls_entity-lastchangedby
                                    lastchangedon	= ls_entity-lastchangedon
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

    DATA: lv_abrufstat_params TYPE zcl_rs_3abrufstatus=>zif_rs_3dpc_abrufstatus~ts_input_parameters,
          ls_kopf_params      TYPE zcl_rs_3abrufkopf=>ts_input_parameters.
    DATA(lo_abrufkopf) = NEW zcl_rs_3abrufkopf( ls_kopf_params ).
    CASE NEW zcl_rs_3abrufstatus( lv_abrufstat_params )->get_abrufkopf_contract( lt_entity[ 1 ] ).
      WHEN zif_rs_3constants=>gc_abruf_contract_lp2.
        DATA(lv_proceszzeart) = zif_rs_3constants=>gc_procezzart_type_abrufpos-lp2.
      WHEN zif_rs_3constants=>gc_abruf_contract_lp3.
        lv_proceszzeart = zif_rs_3constants=>gc_procezzart_type_abrufpos-lp3.
      WHEN zif_rs_3constants=>gc_abruf_contract_lp5.
        lv_proceszzeart = zif_rs_3constants=>gc_procezzart_type_abrufpos-lp5.
      WHEN OTHERS.
        CLEAR lv_proceszzeart.
    ENDCASE.

    DATA(lt_sub_entity) = VALUE tt_deep_abrufpos( FOR ls_entity IN lt_entity
                              (
                          mandt =	ls_entity-mandt
                          abrufnummer	=	ls_entity-abrufnummer
                          abrufposition	=	ls_entity-abrufposition
                          artikelnummer	=	ls_entity-artikelnummer
                          gueltig_ab  = ls_entity-gueltig_ab
                          referenz  = ls_entity-referenz
                          wert_objekt	=	ls_entity-wert_objekt
                          einheit_wert  = ls_entity-einheit_wert
                          menge	=	ls_entity-menge
                          wert_position	=	ls_entity-wert_position
                          kosten_position	=	ls_entity-kosten_position
                          waehrung  = ls_entity-waehrung
                          geplantes_lieferdatum	=	ls_entity-geplantes_lieferdatum
                          realisiertes_lieferdatum  = ls_entity-realisiertes_lieferdatum
                          abgerechnet	=	ls_entity-abgerechnet
                          storniert	=	ls_entity-storniert
                          datum_storno  = ls_entity-datum_storno
                          abrufer	=	ls_entity-abrufer
                          abrufdatum  = ls_entity-abrufdatum
                          rueckruf_datum  = ls_entity-rueckruf_datum
                          rueckruf_grund  = ls_entity-rueckruf_grund
                          abnahme_status  = ls_entity-abnahme_status
                          notiz	=	ls_entity-notiz
                          geloescht	=	ls_entity-geloescht
                          attachment  = ls_entity-attachment
                          lastchangedby	=	ls_entity-lastchangedby
                          lastchangedon	=	ls_entity-lastchangedon
                          status_desc  = lo_abrufkopf->get_statusabruf_descr( is_value = ls_entity-abnahme_status
                                                                              is_prozessart = lv_proceszzeart )
                          abrufposition_int = CONV #( ls_entity-abrufposition )
                          "expand to status set
                          tostatusset	= get_statuset( is_abrufnummer = ls_entity-abrufnummer
                                                      is_status = ls_entity-abnahme_status )
                          ) ).

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
*       set timestamp
      DATA lv_timestamp_reset TYPE timestamp.
      GET TIME STAMP FIELD lv_timestamp_reset.
      DATA(lv_secs) = zif_rs_3constants=>gc_session_expires_secs-position * zif_rs_3constants=>gc_session_expires_secs-multiplier.

      lv_timestamp_reset = cl_abap_tstmp=>subtractsecs( tstmp  = lv_timestamp_reset
                                                        secs = lv_secs ).
      ls_entity-lastchangedon = lv_timestamp_reset.

*        release the locking
      DATA lt_abrufpos TYPE tt_t_abrufpos.
      APPEND map_entity_to_db_struc( ls_entity ) TO lt_abrufpos.
      submit_db_values( lt_abrufpos ).

      IF ls_entity-abrufnummer IS NOT INITIAL AND
        ls_entity-abrufposition IS NOT INITIAL.
        DATA(lv_osql_where_clause) = mc_filter_values-to_read_keys.
        REPLACE ALL OCCURRENCES: OF mc_filter_values-ampersand IN lv_osql_where_clause WITH ls_entity-abrufnummer,
                                 OF mc_filter_values-at_sign IN lv_osql_where_clause WITH ls_entity-abrufposition.
        CONDENSE lv_osql_where_clause.
*
        lt_entity = get_db_values( lv_osql_where_clause ).
      ENDIF.

      IF lt_entity IS NOT INITIAL.
        REFRESH rv_et_entity.
        rv_et_entity = lt_entity.
      ENDIF.
      RETURN.
    ENDIF.
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
      lv_session_time = CONV tzntstmpl( zif_rs_3constants=>gc_session_expires_secs-position ).
      IF lv_diffsecs <  lv_session_time.
        raise_error( is_message = |Wird derzeit von bearbeitet { ls_entity-lastchangedby }| ). " Currently being edited by
      ENDIF.
    ENDIF.

    ls_entity-lastchangedby = lv_uname.
    ls_entity-lastchangedon = lv_timestamp_now.
    ls_entity-is_edit = abap_true.

    APPEND map_entity_to_db_struc( ls_entity ) TO lt_abrufpos.
    submit_db_values( lt_abrufpos ).

    IF ls_entity-abrufnummer IS NOT INITIAL AND
      ls_entity-abrufposition IS NOT INITIAL.
      lv_osql_where_clause = mc_filter_values-to_read_keys.
      REPLACE ALL OCCURRENCES: OF mc_filter_values-ampersand IN lv_osql_where_clause WITH ls_entity-abrufnummer,
                               OF mc_filter_values-at_sign IN lv_osql_where_clause WITH ls_entity-abrufposition.
      CONDENSE lv_osql_where_clause.
*
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

    DATA: lv_abrufstat_params TYPE zcl_rs_3abrufstatus=>zif_rs_3dpc_abrufstatus~ts_input_parameters,
          ls_kopf_params      TYPE zcl_rs_3abrufkopf=>ts_input_parameters.
    DATA(lo_abrufkopf) = NEW zcl_rs_3abrufkopf( ls_kopf_params ).
    CASE NEW zcl_rs_3abrufstatus( lv_abrufstat_params )->get_abrufkopf_contract( lt_entity[ 1 ] ).
      WHEN zif_rs_3constants=>gc_abruf_contract_lp2.
        DATA(lv_proceszzeart) = zif_rs_3constants=>gc_procezzart_type_abrufpos-lp2.
      WHEN zif_rs_3constants=>gc_abruf_contract_lp3.
        lv_proceszzeart = zif_rs_3constants=>gc_procezzart_type_abrufpos-lp3.
      WHEN zif_rs_3constants=>gc_abruf_contract_lp5.
        lv_proceszzeart = zif_rs_3constants=>gc_procezzart_type_abrufpos-lp5.
      WHEN OTHERS.
        CLEAR lv_proceszzeart.
    ENDCASE.

    DATA(lt_sub_entity) = VALUE ts_input_parameters-et_entityset( FOR ls_entity IN lt_entity
                            (
                              abrufnummer	= ls_entity-abrufnummer
                              abrufposition	= ls_entity-abrufposition
                              artikelnummer   = ls_entity-artikelnummer
                              gueltig_ab  = ls_entity-gueltig_ab
                              referenz  = ls_entity-referenz
                              wert_objekt	= ls_entity-wert_objekt
                              einheit_wert  = ls_entity-einheit_wert
                              menge	= ls_entity-menge
                              wert_position	= ls_entity-wert_position
                              kosten_position	= ls_entity-kosten_position
                              waehrung  = ls_entity-waehrung
                              geplantes_lieferdatum	= ls_entity-geplantes_lieferdatum
                              realisiertes_lieferdatum  = ls_entity-realisiertes_lieferdatum
                              abgerechnet	= ls_entity-abgerechnet
                              storniert	= ls_entity-storniert
                              datum_storno  = ls_entity-datum_storno
                              abrufer	= ls_entity-abrufer
                              abrufdatum  = ls_entity-abrufdatum
                              rueckruf_datum  = ls_entity-rueckruf_datum
                              rueckruf_grund  = ls_entity-rueckruf_grund
                              abnahme_status  = ls_entity-abnahme_status
                              notiz	= ls_entity-notiz
                              geloescht	= ls_entity-geloescht
                              attachment  = ls_entity-attachment
                              lastchangedby	= ls_entity-lastchangedby
                              lastchangedon	= ls_entity-lastchangedon
                              is_edit	= ls_entity-is_edit
                              status_desc  = lo_abrufkopf->get_statusabruf_descr( is_value = ls_entity-abnahme_status
                                                                             is_prozessart = lv_proceszzeart )
                              abrufposition_int = CONV #( ls_entity-abrufposition )
                                )
                               ).

    IF lt_sub_entity IS NOT INITIAL.
      REFRESH rv_et_entity.
      rv_et_entity = lt_sub_entity.
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
    DATA(lt_abrufpos) = is_t_abrufpos.
    DATA lv_message TYPE string.

    IF lines( lt_abrufpos ) >= 1.
      "insert value/update data
      MODIFY zrs_3tt_abrufpos FROM TABLE lt_abrufpos.
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
          lv_message TYPE string,
          lt_entity  TYPE tt_t_abrufpos.
    IF gs_params-io_data_provider IS BOUND.
      gs_params-io_data_provider->read_entry_data( IMPORTING es_data = ls_entity ).
    ENDIF.
    IF ls_entity IS INITIAL.
      RETURN.
    ENDIF.
    gs_params-er_entity = ls_entity.

* Check status
    DATA(lo_status) = zcl_rs_3abrufstatus=>create( VALUE zif_rs_3dpc_abrufstatus=>ts_input_parameters(
                                                                     is_cud_type  = zif_rs_3constants=>gc_cud_types-create
                                                                     iv_entity_set_name = gs_params-iv_entity_set_name
                                                                     er_entity_pos = ls_entity
                                                                     es_mo_context = gs_params-mo_context ) ).
    lo_status->check( ).
    lo_status->get_results( IMPORTING es_entity_vorhaben = DATA(ls_entity_vorhaben)
                                      es_entity_kopf = DATA(ls_entity_kopf)
                                      es_entity_pos = DATA(ls_entity_pos)
                                      es_entity_pos_old = DATA(ls_entity_pos_old)
                                      es_entity_billing = DATA(ls_entity_billing)
                                      es_email_params = DATA(ls_email_params)
                                      es_mo_context = gs_params-mo_context  ).

*  abrufpos handling
   gs_params-er_entity = handle_abrufpos( ls_entity_pos ).

* abrufkopf handling
    handle_abrufkopf( ls_entity_kopf ).

* billing handling
    handle_verrchng( ls_entity_billing ).

*  email handling
    ls_email_params = handle_email( ls_email_params ).

    DATA(lo_response) = NEW zcl_rs_3abrufpos( gs_params ).
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
    DATA(lo_response) = NEW zcl_rs_3abrufpos( gs_params ).
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
    DATA(lo_response) = NEW zcl_rs_3abrufpos( gs_params ).
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

    DATA(lv_osql_where_clause) = mc_filter_values-to_read_keys.
    DATA(lt_keys) = gs_params-io_tech_request_context_read->get_keys( ).
    IF NOT line_exists( lt_keys[ name = mc_key_field ] ) OR lt_keys IS INITIAL.
      RETURN.
    ENDIF.
    IF NOT line_exists( lt_keys[ name = mc_key_sec_field ] ).
      RETURN.
    ENDIF.

    DATA(lv_abrufnummer) = lt_keys[ name = mc_key_field ]-value.
    DATA(lv_abrufposition) = lt_keys[ name = mc_key_sec_field ]-value.

    REPLACE ALL OCCURRENCES: OF mc_filter_values-ampersand IN lv_osql_where_clause WITH lv_abrufnummer,
                             OF mc_filter_values-at_sign IN lv_osql_where_clause WITH lv_abrufposition.
    CONDENSE lv_osql_where_clause.


    DATA(lt_entity) = get_db_values( lv_osql_where_clause ).
    lt_entity = set_status_descriptions( lt_entity ).

    IF lt_entity IS NOT INITIAL.
      gs_params-er_entity = lt_entity[ 1 ].
    ENDIF.

    DATA(lo_response) = NEW zcl_rs_3abrufpos( gs_params ).
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

    DATA(lv_osql_where_clause) = mc_filter_values-to_read_keys.
    DATA(lt_keys) = gs_params-io_tech_request_context_read->get_keys( ).
    IF NOT line_exists( lt_keys[ name = mc_key_field ] ) OR lt_keys IS INITIAL.
      RETURN.
    ENDIF.
    IF NOT line_exists( lt_keys[ name = mc_key_sec_field ] ).
      RETURN.
    ENDIF.

    DATA(lv_abrufnummer) = lt_keys[ name = mc_key_field ]-value.
    DATA(lv_abrufposition) = lt_keys[ name = mc_key_sec_field ]-value.

    REPLACE ALL OCCURRENCES: OF mc_filter_values-ampersand IN lv_osql_where_clause WITH lv_abrufnummer,
                             OF mc_filter_values-at_sign IN lv_osql_where_clause WITH lv_abrufposition.
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

    DATA(lo_response) = NEW zcl_rs_3abrufpos( gs_params ).
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
    DATA ls_entity LIKE gs_params-er_entity.
    IF gs_params-io_data_provider IS BOUND.
      gs_params-io_data_provider->read_entry_data( IMPORTING es_data = ls_entity ).
    ENDIF.
    IF ls_entity IS INITIAL.
      RETURN.
    ENDIF.
    gs_params-er_entity = ls_entity.

* Check status
    DATA(lo_status) = zcl_rs_3abrufstatus=>create( VALUE zif_rs_3dpc_abrufstatus=>ts_input_parameters(
                                                                     is_cud_type  = zif_rs_3constants=>gc_cud_types-update
                                                                     iv_entity_set_name = gs_params-iv_entity_set_name
                                                                     er_entity_pos = ls_entity
                                                                     es_mo_context = gs_params-mo_context ) ).
    lo_status->check( ).
    lo_status->get_results( IMPORTING es_entity_vorhaben = DATA(ls_entity_vorhaben)
                                      es_entity_kopf = DATA(ls_entity_kopf)
                                      es_entity_pos = DATA(ls_entity_pos)
                                      es_entity_pos_old = DATA(ls_entity_pos_old)
                                      es_entity_billing = DATA(ls_entity_billing)
                                      es_email_params = DATA(ls_email_params)
                                      es_mo_context = gs_params-mo_context  ).
*  abrufpos handling
    gs_params-er_entity = handle_abrufpos( ls_entity_pos ).

* abrufkopf handling
    handle_abrufkopf( ls_entity_kopf ).

* billing handling
    handle_verrchng( ls_entity_billing ).

*  email handling
    ls_email_params = handle_email( ls_email_params ).

    DATA(lo_response) = NEW zcl_rs_3abrufpos( gs_params ).
    rv_if_response = lo_response.
  ENDMETHOD.
ENDCLASS.
