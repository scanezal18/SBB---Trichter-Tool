class ZCL_ZRS_3FUNNELTOOL_UI_DPC_EXT definition
  public
  inheriting from ZCL_ZRS_3FUNNELTOOL_UI_DPC
  create public .

public section.

  methods /IWBEP/IF_MGW_APPL_SRV_RUNTIME~CHANGESET_BEGIN
    redefinition .
  methods /IWBEP/IF_MGW_APPL_SRV_RUNTIME~CHANGESET_END
    redefinition .
  methods /IWBEP/IF_MGW_APPL_SRV_RUNTIME~CHANGESET_PROCESS
    redefinition .
  methods /IWBEP/IF_MGW_APPL_SRV_RUNTIME~GET_EXPANDED_ENTITY
    redefinition .
  methods /IWBEP/IF_MGW_APPL_SRV_RUNTIME~GET_EXPANDED_ENTITYSET
    redefinition .
  methods /IWBEP/IF_MGW_APPL_SRV_RUNTIME~GET_IS_CONDITIONAL_IMPLEMENTED
    redefinition .
protected section.

  methods ABRUFKOPFSET_CREATE_ENTITY
    redefinition .
  methods ABRUFKOPFSET_GET_ENTITY
    redefinition .
  methods ABRUFKOPFSET_GET_ENTITYSET
    redefinition .
  methods ABRUFKOPFSET_UPDATE_ENTITY
    redefinition .
  methods ABRUFPOSSET_CREATE_ENTITY
    redefinition .
  methods ABRUFPOSSET_GET_ENTITY
    redefinition .
  methods ABRUFPOSSET_GET_ENTITYSET
    redefinition .
  methods ABRUFPOSSET_UPDATE_ENTITY
    redefinition .
  methods BENUTZERROLLENSE_GET_ENTITY
    redefinition .
  methods BENUTZERROLLENSE_GET_ENTITYSET
    redefinition .
  methods KATALOGKATEGORIE_GET_ENTITY
    redefinition .
  methods KATALOGKATEGORIE_GET_ENTITYSET
    redefinition .
  methods KATALOGKOMPLEXIT_GET_ENTITY
    redefinition .
  methods KATALOGKOMPLEXIT_GET_ENTITYSET
    redefinition .
  methods KATALOGLIEFEROBJ_GET_ENTITY
    redefinition .
  methods KATALOGLIEFEROBJ_GET_ENTITYSET
    redefinition .
  methods KATALOGPREISSET_GET_ENTITY
    redefinition .
  methods MDARTIKELSET_CREATE_ENTITY
    redefinition .
  methods MDARTIKELSET_GET_ENTITY
    redefinition .
  methods MDARTIKELSET_GET_ENTITYSET
    redefinition .
  methods MDARTIKELSET_UPDATE_ENTITY
    redefinition .
  methods PROJEKTDROPDOWNL_GET_ENTITYSET
    redefinition .
  methods REFERENZEINHEITW_GET_ENTITY
    redefinition .
  methods REFERENZEINHEITW_GET_ENTITYSET
    redefinition .
  methods REFERENZEINZELVE_GET_ENTITY
    redefinition .
  methods REFERENZEINZELVE_GET_ENTITYSET
    redefinition .
  methods REFERENZPROJECTA_GET_ENTITY
    redefinition .
  methods REFERENZPROJECTA_GET_ENTITYSET
    redefinition .
  methods SUCHEBENUTZERSET_GET_ENTITYSET
    redefinition .
  methods VERRECHNUNGSELEM_GET_ENTITYSET
    redefinition .
  methods VORHABENFORECAST_GET_ENTITY
    redefinition .
  methods VORHABENFORECAST_GET_ENTITYSET
    redefinition .
  methods VORHABENFORECAST_UPDATE_ENTITY
    redefinition .
  methods VORHABENSET_CREATE_ENTITY
    redefinition .
  methods VORHABENSET_GET_ENTITY
    redefinition .
  methods VORHABENSET_GET_ENTITYSET
    redefinition .
  methods VORHABENSET_UPDATE_ENTITY
    redefinition .
  methods KATALOGPREISSET_GET_ENTITYSET
    redefinition .
private section.
ENDCLASS.



CLASS ZCL_ZRS_3FUNNELTOOL_UI_DPC_EXT IMPLEMENTATION.


  METHOD /iwbep/if_mgw_appl_srv_runtime~changeset_begin.
*----------------------------------------------------------------------*
* SBB AG, SAP CCC-IT, Bern/Berne/Berna
*----------------------------------------------------------------------*
*
*-----------Änderungsdokumentation / documentation of changes----------*
* Version SAP-Rel Datum Bearbeiter
* Beschreibung Flag for forecast batch update
*----------------------------------------------------------------------*
* >000< 7.50 03.11.2020 Firma(DXC Technology) / Name(Leonardo Ramos)
* Programmerstellung / creation of program
*----------------------------------------------------------------------*
* >001< 7.50 tt.mm.jjjj Firma(company) / Name(name)
* ...
*----------------------------------------------------------------------*
    IF it_operation_info[] IS NOT INITIAL.
      LOOP AT it_operation_info INTO DATA(ls_operation_info).

        CASE ls_operation_info-entity_set.

          WHEN zif_rs_3constants=>gc_entityset_vorhabenforecast.
            IF ls_operation_info-operation_type EQ zif_rs_3constants=>gc_update_entity.
              cv_defer_mode = abap_true.
            ELSE.
              CLEAR: cv_defer_mode.
            ENDIF.

          WHEN OTHERS.

        ENDCASE.

      ENDLOOP.
    ENDIF.

  ENDMETHOD.


  METHOD /iwbep/if_mgw_appl_srv_runtime~changeset_end.
*----------------------------------------------------------------------*
* SBB AG, SAP CCC-IT, Bern/Berne/Berna
*----------------------------------------------------------------------*
*
*-----------Änderungsdokumentation / documentation of changes----------*
* Version SAP-Rel Datum Bearbeiter
* Beschreibung Commit work for forecast batch update
*----------------------------------------------------------------------*
* >000< 7.50 03.11.2020 Firma(DXC Technology) / Name(Leonardo Ramos)
* Programmerstellung / creation of program
*----------------------------------------------------------------------*
* >001< 7.50 tt.mm.jjjj Firma(company) / Name(name)
* ...
*----------------------------------------------------------------------*

    COMMIT WORK.

  ENDMETHOD.


  METHOD /iwbep/if_mgw_appl_srv_runtime~changeset_process.
*----------------------------------------------------------------------*
* SBB AG, SAP CCC-IT, Bern/Berne/Berna
*----------------------------------------------------------------------*
*
*-----------Änderungsdokumentation / documentation of changes----------*
* Version SAP-Rel Datum Bearbeiter
* Beschreibung Forecast batch update values for all edited months
*----------------------------------------------------------------------*
* >000< 7.50 03.11.2020 Firma(DXC Technology) / Name(Leonardo Ramos)
* Programmerstellung / creation of program
*----------------------------------------------------------------------*
* >001< 7.50 tt.mm.jjjj Firma(company) / Name(name)
* ...
*----------------------------------------------------------------------*

    DATA: ls_changeset_request  TYPE /iwbep/if_mgw_appl_types=>ty_s_changeset_request,
          ls_changeset_response TYPE /iwbep/if_mgw_appl_types=>ty_s_changeset_response,
          lv_entity_set         TYPE string,
          lo_create_context     TYPE REF TO /iwbep/if_mgw_req_entity_u,
          lt_data               TYPE STANDARD TABLE OF zrs_3tt_projfcst,
          ls_data               TYPE zrs_3tt_projfcst,
          lv_aw                 TYPE zrs_3tt_vorhaben-projekt_forecast_aw,
          lv_tp                 TYPE zrs_3tt_vorhaben-projekt_forecast_tp,
          lv_message            TYPE string.
    FIELD-SYMBOLS: <ls_forecst> TYPE zrs_3tt_projfcst.

    CLEAR: ls_changeset_request, ls_data.
    REFRESH: lt_data.

    LOOP AT it_changeset_request INTO ls_changeset_request.
      lo_create_context ?= ls_changeset_request-request_context.
      lv_entity_set = lo_create_context->get_entity_set_name( ).

      CASE lv_entity_set.
*        WHEN 'VorhabenforecastSet'.
        WHEN zif_rs_3constants=>gc_entityset_vorhabenforecast.
          IF ls_changeset_request-operation_type = zif_rs_3constants=>gc_update_entity
            OR ls_changeset_request-operation_type = zif_rs_3constants=>gc_patch_entity.
            ls_changeset_request-entry_provider->read_entry_data( IMPORTING es_data = ls_data ).
            SELECT SINGLE *
              FROM zrs_3tt_projfcst
              INTO @DATA(ls_projfcst)
              WHERE vorhabensnummer EQ @ls_data-vorhabensnummer
                AND kalender_jahr_monat EQ @ls_data-kalender_jahr_monat.
            IF sy-subrc EQ 0.
              ls_data-mandt = ls_projfcst-mandt.
              ls_data-einheit_forecast_aw = ls_projfcst-einheit_forecast_aw.
              ls_data-einheit_forecast_tp = ls_projfcst-einheit_forecast_tp.
              copy_data_to_ref( EXPORTING is_data = ls_data
                                CHANGING cr_data = ls_changeset_response-entity_data ) .
              "collect update data
              APPEND ls_data TO lt_data.
              ls_changeset_response-operation_no = ls_changeset_request-operation_no.
              APPEND ls_changeset_response TO ct_changeset_response.
              CLEAR: ls_changeset_request.
            ENDIF.
          ENDIF.

        WHEN OTHERS.
      ENDCASE.

    ENDLOOP.

    "update project forecast and project header based from request updates
    IF lt_data[] IS NOT INITIAL.
      SORT lt_data BY vorhabensnummer kalender_jahr_monat.
      MODIFY zrs_3tt_projfcst
        FROM TABLE lt_data.

      READ TABLE lt_data INTO ls_data
        INDEX 1.
      "get project header entry
      SELECT SINGLE *
        FROM zrs_3tt_vorhaben
        INTO @DATA(ls_vorhaben)
        WHERE vorhabensnummer EQ @ls_data-vorhabensnummer.
      IF sy-subrc EQ 0.
        "check first for existing entries in forecast table incase
        "of change in project header dates
        SELECT *
          FROM zrs_3tt_projfcst
          INTO TABLE @DATA(lt_projfcst)
          WHERE vorhabensnummer EQ @ls_vorhaben-vorhabensnummer.
        IF sy-subrc EQ 0.
          UNASSIGN: <ls_forecst>.
          CLEAR: lv_aw, lv_tp, ls_data.
          "calculate total forecast for TP and AW
          LOOP AT lt_projfcst ASSIGNING <ls_forecst>.
            READ TABLE lt_data INTO ls_data
              WITH KEY vorhabensnummer = <ls_forecst>-vorhabensnummer
                       kalender_jahr_monat = <ls_forecst>-kalender_jahr_monat.
            IF sy-subrc EQ 0.
              <ls_forecst>-forecast_aw = ls_data-forecast_aw.
              <ls_forecst>-forecast_tp = ls_data-forecast_tp.
            ENDIF.
            lv_aw = lv_aw + <ls_forecst>-forecast_aw.
            lv_tp = lv_tp + <ls_forecst>-forecast_tp.
          ENDLOOP.
        ENDIF.

        ls_vorhaben-projekt_forecast_aw = lv_aw.
        ls_vorhaben-projekt_forecast_tp = lv_tp.

        "check if forecast is maintained or not, then change project
        "header status accordingly
        IF lv_aw NE 0 OR lv_tp NE 0.
          IF ls_vorhaben-status EQ zif_rs_3constants=>gc_vorhaben_status-sv0001. "Vorabinfo
            ls_vorhaben-status = zif_rs_3constants=>gc_vorhaben_status-sv0002. "In vorbereitung
          ENDIF.
        ELSEIF lv_aw EQ 0 AND lv_tp EQ 0.
          IF ls_vorhaben-status = zif_rs_3constants=>gc_vorhaben_status-sv0002. "In vorbereitung
            ls_vorhaben-status = zif_rs_3constants=>gc_vorhaben_status-sv0001. "Vorabinfo
          ENDIF.
        ENDIF.

        "modify the proj header values
        MODIFY zrs_3tt_vorhaben FROM ls_vorhaben.
        IF sy-subrc NE 0.
          MESSAGE i005(zrs_3) WITH ls_vorhaben-vorhabensnummer INTO lv_message.
*          raise_error( is_message = lv_message ).
        ENDIF.
      ENDIF.

    ENDIF.

  ENDMETHOD.


 METHOD /iwbep/if_mgw_appl_srv_runtime~get_expanded_entity.
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

   DATA(lv_entityset_name) = io_tech_request_context->get_entity_set_name( ).

   CASE lv_entityset_name.

     WHEN zif_rs_3constants=>gc_entityset_benutzerrollenset.

       DATA(lo_entitat) = zcl_rs_3benutzerollen=>create(
          VALUE zcl_rs_3benutzerollen=>ts_input_parameters(
                            iv_entity_name           = iv_entity_name
                            iv_entity_set_name       = iv_entity_set_name
                            iv_source_name           = iv_source_name
                            it_key_tab               = it_key_tab
                            it_navigation_path       = it_navigation_path
                            io_expand = io_expand
                            io_tech_request_context_read  = io_tech_request_context
                            er_entity_expand = er_entity
                            et_expanded_clauses = et_expanded_clauses
                            et_expanded_tech_clauses = et_expanded_tech_clauses
                            es_response_context_read =   es_response_context
                            mo_context = mo_context
                              ) ).
       lo_entitat->read_expand( ).
       DATA(lo_antwort) = CAST zif_rs_3dpc_antwort( lo_entitat ).
       lo_antwort->get_response_read_expand( IMPORTING es_entity = er_entity
                                                       es_t_expanded_clauses = et_expanded_clauses
                                                       es_t_expanded_tech_clauses = et_expanded_tech_clauses
                                                       es_response_context = es_response_context
                                                       es_mo_context = mo_context ).

     WHEN zif_rs_3constants=>gc_entityset_abrufkopfset.

       lo_entitat = zcl_rs_3abrufkopf=>create(
          VALUE zcl_rs_3abrufkopf=>ts_input_parameters(
                            iv_entity_name           = iv_entity_name
                            iv_entity_set_name       = iv_entity_set_name
                            iv_source_name           = iv_source_name
                            it_key_tab               = it_key_tab
                            it_navigation_path       = it_navigation_path
                            io_expand = io_expand
                            io_tech_request_context_read  = io_tech_request_context
                            er_entity_expand = er_entity
                            et_expanded_clauses = et_expanded_clauses
                            et_expanded_tech_clauses = et_expanded_tech_clauses
                            es_response_context_read =   es_response_context
                            mo_context = mo_context
                              ) ).
       lo_entitat->read_expand( ).
       lo_antwort = CAST zif_rs_3dpc_antwort( lo_entitat ).
       lo_antwort->get_response_read_expand( IMPORTING es_entity = er_entity
                                                       es_t_expanded_clauses = et_expanded_clauses
                                                       es_t_expanded_tech_clauses = et_expanded_tech_clauses
                                                       es_response_context = es_response_context
                                                       es_mo_context = mo_context ).

     WHEN zif_rs_3constants=>gc_entityset_abrufposset.

       lo_entitat = zcl_rs_3abrufpos=>create(
          VALUE zcl_rs_3abrufpos=>ts_input_parameters(
                            iv_entity_name           = iv_entity_name
                            iv_entity_set_name       = iv_entity_set_name
                            iv_source_name           = iv_source_name
                            it_key_tab               = it_key_tab
                            it_navigation_path       = it_navigation_path
                            io_expand = io_expand
                            io_tech_request_context_read  = io_tech_request_context
                            er_entity_expand = er_entity
                            et_expanded_clauses = et_expanded_clauses
                            et_expanded_tech_clauses = et_expanded_tech_clauses
                            es_response_context_read =   es_response_context
                            mo_context = mo_context
                              ) ).
       lo_entitat->read_expand( ).
       lo_antwort = CAST zif_rs_3dpc_antwort( lo_entitat ).
       lo_antwort->get_response_read_expand( IMPORTING es_entity = er_entity
                                                       es_t_expanded_clauses = et_expanded_clauses
                                                       es_t_expanded_tech_clauses = et_expanded_tech_clauses
                                                       es_response_context = es_response_context
                                                       es_mo_context = mo_context ).

     WHEN zif_rs_3constants=>gc_entityset_mdartikelset.

       lo_entitat = zcl_rs_3mdartikel=>create(
          VALUE zcl_rs_3mdartikel=>ts_input_parameters(
                            iv_entity_name           = iv_entity_name
                            iv_entity_set_name       = iv_entity_set_name
                            iv_source_name           = iv_source_name
                            it_key_tab               = it_key_tab
                            it_navigation_path       = it_navigation_path
                            io_expand = io_expand
                            io_tech_request_context_read  = io_tech_request_context
                            er_entity_expand = er_entity
                            et_expanded_clauses = et_expanded_clauses
                            et_expanded_tech_clauses = et_expanded_tech_clauses
                            es_response_context_read =   es_response_context
                            mo_context = mo_context
                              ) ).
       lo_entitat->read_expand( ).
       lo_antwort = CAST zif_rs_3dpc_antwort( lo_entitat ).
       lo_antwort->get_response_read_expand( IMPORTING es_entity = er_entity
                                                       es_t_expanded_clauses = et_expanded_clauses
                                                       es_t_expanded_tech_clauses = et_expanded_tech_clauses
                                                       es_response_context = es_response_context
                                                       es_mo_context = mo_context ).
     WHEN OTHERS.

       super->/iwbep/if_mgw_appl_srv_runtime~get_expanded_entity(
         EXPORTING
           iv_entity_name           = iv_entity_name
           iv_entity_set_name       = iv_entity_set_name
           iv_source_name           = iv_source_name
           it_key_tab               = it_key_tab
           it_navigation_path       = it_navigation_path
           io_expand                = io_expand
           io_tech_request_context  = io_tech_request_context
         IMPORTING
           er_entity                = er_entity
           es_response_context      = es_response_context
           et_expanded_clauses      = et_expanded_clauses
           et_expanded_tech_clauses = et_expanded_tech_clauses ).
   ENDCASE.

 ENDMETHOD.


  METHOD /iwbep/if_mgw_appl_srv_runtime~get_expanded_entityset.
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

    DATA(lv_entityset_name) = io_tech_request_context->get_entity_set_name( ).

    CASE lv_entityset_name.

      WHEN zif_rs_3constants=>gc_entityset_benutzerrollenset.

        DATA(lo_entitat) = zcl_rs_3benutzerollen=>create(
            VALUE zcl_rs_3benutzerollen=>ts_input_parameters(
            iv_entity_name = iv_entity_name
            iv_entity_set_name = iv_entity_set_name
            iv_source_name = iv_source_name
            it_filter_select_options = it_filter_select_options
            is_paging = is_paging
            it_key_tab = it_key_tab
            it_navigation_path = it_navigation_path
            it_order = it_order
            iv_filter_string = iv_filter_string
            iv_search_string = iv_search_string
            io_expand = io_expand
            io_tech_request_context_query = io_tech_request_context
            er_entityset_expand = er_entityset
            et_expanded_clauses = et_expanded_clauses
            et_expanded_tech_clauses = et_expanded_tech_clauses
            es_response_context_set = es_response_context
            mo_context = mo_context
            ) ).
        lo_entitat->query_expand( ).
        DATA(lo_antwort) = CAST zif_rs_3dpc_antwort( lo_entitat ).
        lo_antwort->get_response_query_expand( IMPORTING es_entityset = er_entityset
        es_t_expanded_clauses = et_expanded_clauses
        es_t_expanded_tech_clauses = et_expanded_tech_clauses
        es_response_context = es_response_context
        es_mo_context = mo_context ).

      WHEN zif_rs_3constants=>gc_entityset_abrufkopfset.

        lo_entitat = zcl_rs_3abrufkopf=>create(
            VALUE zcl_rs_3abrufkopf=>ts_input_parameters(
            iv_entity_name = iv_entity_name
            iv_entity_set_name = iv_entity_set_name
            iv_source_name = iv_source_name
            it_filter_select_options = it_filter_select_options
            is_paging = is_paging
            it_key_tab = it_key_tab
            it_navigation_path = it_navigation_path
            it_order = it_order
            iv_filter_string = iv_filter_string
            iv_search_string = iv_search_string
            io_expand = io_expand
            io_tech_request_context_query = io_tech_request_context
            er_entityset_expand = er_entityset
            et_expanded_clauses = et_expanded_clauses
            et_expanded_tech_clauses = et_expanded_tech_clauses
            es_response_context_set = es_response_context
            mo_context = mo_context
            ) ).
        lo_entitat->query_expand( ).
        lo_antwort = CAST zif_rs_3dpc_antwort( lo_entitat ).
        lo_antwort->get_response_query_expand( IMPORTING es_entityset = er_entityset
        es_t_expanded_clauses = et_expanded_clauses
        es_t_expanded_tech_clauses = et_expanded_tech_clauses
        es_response_context = es_response_context
        es_mo_context = mo_context ).

      WHEN zif_rs_3constants=>gc_entityset_abrufposset.

        lo_entitat = zcl_rs_3abrufpos=>create(
            VALUE zcl_rs_3abrufpos=>ts_input_parameters(
            iv_entity_name = iv_entity_name
            iv_entity_set_name = iv_entity_set_name
            iv_source_name = iv_source_name
            it_filter_select_options = it_filter_select_options
            is_paging = is_paging
            it_key_tab = it_key_tab
            it_navigation_path = it_navigation_path
            it_order = it_order
            iv_filter_string = iv_filter_string
            iv_search_string = iv_search_string
            io_expand = io_expand
            io_tech_request_context_query = io_tech_request_context
            er_entityset_expand = er_entityset
            et_expanded_clauses = et_expanded_clauses
            et_expanded_tech_clauses = et_expanded_tech_clauses
            es_response_context_set = es_response_context
            mo_context = mo_context
            ) ).
        lo_entitat->query_expand( ).
        lo_antwort = CAST zif_rs_3dpc_antwort( lo_entitat ).
        lo_antwort->get_response_query_expand( IMPORTING es_entityset = er_entityset
        es_t_expanded_clauses = et_expanded_clauses
        es_t_expanded_tech_clauses = et_expanded_tech_clauses
        es_response_context = es_response_context
        es_mo_context = mo_context ).

      WHEN zif_rs_3constants=>gc_entityset_mdartikelset.

        lo_entitat = zcl_rs_3mdartikel=>create(
            VALUE zcl_rs_3mdartikel=>ts_input_parameters(
            iv_entity_name = iv_entity_name
            iv_entity_set_name = iv_entity_set_name
            iv_source_name = iv_source_name
            it_filter_select_options = it_filter_select_options
            is_paging = is_paging
            it_key_tab = it_key_tab
            it_navigation_path = it_navigation_path
            it_order = it_order
            iv_filter_string = iv_filter_string
            iv_search_string = iv_search_string
            io_expand = io_expand
            io_tech_request_context_query = io_tech_request_context
            er_entityset_expand = er_entityset
            et_expanded_clauses = et_expanded_clauses
            et_expanded_tech_clauses = et_expanded_tech_clauses
            es_response_context_set = es_response_context
            mo_context = mo_context
            ) ).
        lo_entitat->query_expand( ).
        lo_antwort = CAST zif_rs_3dpc_antwort( lo_entitat ).
        lo_antwort->get_response_query_expand( IMPORTING es_entityset = er_entityset
        es_t_expanded_clauses = et_expanded_clauses
        es_t_expanded_tech_clauses = et_expanded_tech_clauses
        es_response_context = es_response_context
        es_mo_context = mo_context ).

      WHEN OTHERS.

    ENDCASE.

  ENDMETHOD.


  METHOD /iwbep/if_mgw_appl_srv_runtime~get_is_conditional_implemented.
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
    CASE iv_entity_set_name.
      WHEN zif_rs_3constants=>gc_entityset_abrufkopfset OR
          zif_rs_3constants=>gc_entityset_abrufposset.
        CASE iv_operation_type.
          WHEN /iwbep/if_mgw_appl_types=>gcs_operation_type-update_entity OR
              /iwbep/if_mgw_appl_types=>gcs_operation_type-patch_entity.
            rv_conditional_active = abap_true.
          WHEN OTHERS.
            rv_conditional_active = abap_false.
        ENDCASE.
      WHEN OTHERS.
        rv_conditional_active = abap_false.
    ENDCASE.
  ENDMETHOD.


  METHOD abrufkopfset_create_entity.
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
    DATA(lo_entitat) = zcl_rs_3abrufkopf=>create(
           VALUE zcl_rs_3abrufkopf=>ts_input_parameters(
                             iv_entity_name           = iv_entity_name
                             iv_entity_set_name       = iv_entity_set_name
                             iv_source_name           = iv_source_name
                             it_key_tab               = it_key_tab
                             io_tech_request_context_create = io_tech_request_context
                             it_navigation_path       = it_navigation_path
                             io_data_provider         = io_data_provider
                             er_entity                = er_entity
                             mo_context               = mo_context
                             ) ).
    lo_entitat->post( ).
    DATA(lo_antwort) = CAST zif_rs_3dpc_antwort( lo_entitat ).
    lo_antwort->get_response_post( IMPORTING es_entity = er_entity
                                             es_mo_context = mo_context ).

  ENDMETHOD.


  METHOD abrufkopfset_get_entity.
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
    DATA(lo_entitat) = zcl_rs_3abrufkopf=>create(
           VALUE zcl_rs_3abrufkopf=>ts_input_parameters(
                             iv_entity_name           = iv_entity_name
                             iv_entity_set_name       = iv_entity_set_name
                             iv_source_name           = iv_source_name
                             it_key_tab               = it_key_tab
                             io_request_object_read   = io_request_object
                             io_tech_request_context_read = io_tech_request_context
                             it_navigation_path       = it_navigation_path
                             er_entity                = er_entity
                             es_response_context_read = es_response_context
                             mo_context = mo_context
                             ) ).
    lo_entitat->read( ).
    DATA(lo_antwort) = CAST zif_rs_3dpc_antwort( lo_entitat ).
    lo_antwort->get_response_read( IMPORTING es_entity = er_entity
                                             es_response_context = es_response_context
                                             es_mo_context = mo_context  ).
  ENDMETHOD.


  method ABRUFKOPFSET_GET_ENTITYSET.
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
    DATA(lo_entitat) = zcl_rs_3abrufkopf=>create(
            VALUE zcl_rs_3abrufkopf=>ts_input_parameters(
                              iv_entity_name           = iv_entity_name
                              iv_entity_set_name       = iv_entity_set_name
                              iv_source_name           = iv_source_name
                              it_filter_select_options = it_filter_select_options
                              is_paging                = is_paging
                              it_key_tab               = it_key_tab
                              it_navigation_path       = it_navigation_path
                              it_order                 = it_order
                              iv_filter_string         = iv_filter_string
                              iv_search_string         = iv_search_string
                              io_tech_request_context_query  = io_tech_request_context
                              et_entityset             = et_entityset
                              es_response_context_set  = es_response_context
                              ) ).
    lo_entitat->query( ).
    DATA(lo_antwort) = CAST zif_rs_3dpc_antwort( lo_entitat ).
    lo_antwort->get_response_query( IMPORTING es_t_entity = et_entityset
                                              es_response_context = es_response_context
                                              es_mo_context = mo_context ).
  endmethod.


  METHOD abrufkopfset_update_entity.
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
    DATA(lo_entitat) = zcl_rs_3abrufkopf=>create(
               VALUE zcl_rs_3abrufkopf=>ts_input_parameters(
                                 iv_entity_name           = iv_entity_name
                                 iv_entity_set_name       = iv_entity_set_name
                                 iv_source_name           = iv_source_name
                                 it_key_tab               = it_key_tab
                                 io_tech_request_context_update = io_tech_request_context
                                 it_navigation_path       = it_navigation_path
                                 io_data_provider         = io_data_provider
                                 er_entity                = er_entity
                                 mo_context               = mo_context
                                 ) ).
    lo_entitat->update( ).
    DATA(lo_antwort) = CAST zif_rs_3dpc_antwort( lo_entitat ).
    lo_antwort->get_response_update( IMPORTING es_entity = er_entity
                                               es_mo_context = mo_context ).

  ENDMETHOD.


  METHOD abrufposset_create_entity.
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
    DATA(lo_entitat) = zcl_rs_3abrufpos=>create(
               VALUE zcl_rs_3abrufpos=>ts_input_parameters(
                                 iv_entity_name           = iv_entity_name
                                 iv_entity_set_name       = iv_entity_set_name
                                 iv_source_name           = iv_source_name
                                 it_key_tab               = it_key_tab
                                 io_tech_request_context_create = io_tech_request_context
                                 it_navigation_path       = it_navigation_path
                                 io_data_provider         = io_data_provider
                                 er_entity                = er_entity
                                 mo_context = mo_context
                                 ) ).
    lo_entitat->post( ).
    DATA(lo_antwort) = CAST zif_rs_3dpc_antwort( lo_entitat ).
    lo_antwort->get_response_post( IMPORTING es_entity = er_entity
                                             es_mo_context = mo_context ).
  ENDMETHOD.


  METHOD abrufposset_get_entity.
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
    DATA(lo_entitat) = zcl_rs_3abrufpos=>create(
               VALUE zcl_rs_3abrufpos=>ts_input_parameters(
                                 iv_entity_name           = iv_entity_name
                                 iv_entity_set_name       = iv_entity_set_name
                                 iv_source_name           = iv_source_name
                                 it_key_tab               = it_key_tab
                                 io_request_object_read   = io_request_object
                                 io_tech_request_context_read = io_tech_request_context
                                 it_navigation_path       = it_navigation_path
                                 er_entity                = er_entity
                                 es_response_context_read = es_response_context
                                 mo_context = mo_context
                                 ) ).
    lo_entitat->read( ).
    DATA(lo_antwort) = CAST zif_rs_3dpc_antwort( lo_entitat ).
    lo_antwort->get_response_read( IMPORTING es_entity = er_entity
                                             es_response_context = es_response_context
                                             es_mo_context = mo_context ).
  ENDMETHOD.


  METHOD abrufposset_get_entityset.
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
    DATA(lo_entitat) = zcl_rs_3abrufpos=>create(
                VALUE zcl_rs_3abrufpos=>ts_input_parameters(
                                  iv_entity_name           = iv_entity_name
                                  iv_entity_set_name       = iv_entity_set_name
                                  iv_source_name           = iv_source_name
                                  it_filter_select_options = it_filter_select_options
                                  is_paging                = is_paging
                                  it_key_tab               = it_key_tab
                                  it_navigation_path       = it_navigation_path
                                  it_order                 = it_order
                                  iv_filter_string         = iv_filter_string
                                  iv_search_string         = iv_search_string
                                  io_tech_request_context_query  = io_tech_request_context
                                  et_entityset             = et_entityset
                                  es_response_context_set  = es_response_context
                                  mo_context = mo_context
                                  ) ).
    lo_entitat->query( ).
    DATA(lo_antwort) = CAST zif_rs_3dpc_antwort( lo_entitat ).
    lo_antwort->get_response_query( IMPORTING es_t_entity = et_entityset
                                              es_response_context = es_response_context
                                              es_mo_context = mo_context ).
  ENDMETHOD.


  METHOD abrufposset_update_entity.
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
    DATA(lo_entitat) = zcl_rs_3abrufpos=>create(
                   VALUE zcl_rs_3abrufpos=>ts_input_parameters(
                                     iv_entity_name           = iv_entity_name
                                     iv_entity_set_name       = iv_entity_set_name
                                     iv_source_name           = iv_source_name
                                     it_key_tab               = it_key_tab
                                     io_tech_request_context_update = io_tech_request_context
                                     it_navigation_path       = it_navigation_path
                                     io_data_provider         = io_data_provider
                                     er_entity                = er_entity
                                     mo_context = mo_context
                                     ) ).
    lo_entitat->update( ).
    DATA(lo_antwort) = CAST zif_rs_3dpc_antwort( lo_entitat ).
    lo_antwort->get_response_update( IMPORTING es_entity = er_entity
                                               es_mo_context = mo_context ).
  ENDMETHOD.


  METHOD benutzerrollense_get_entity.
*----------------------------------------------------------------------*
* SBB AG, SAP CCC-IT, Bern/Berne/Berna
*----------------------------------------------------------------------*
*
*-----------Änderungsdokumentation / documentation of changes----------*
* Version SAP-Rel Datum Bearbeiter
* Beschreibung Role Entity Set
*----------------------------------------------------------------------*
* >000< 7.50 28.10.2020 Firma(DXC Technology) / Name(Shernan Bamba)
* Programmerstellung / creation of program
*----------------------------------------------------------------------*
* >001< 7.50 tt.mm.jjjj Firma(company) / Name(name)
* ...
*----------------------------------------------------------------------*

    DATA(lo_entitat) = zcl_rs_3benutzerollen=>create(
           VALUE zcl_rs_3benutzerollen=>ts_input_parameters(
                             iv_entity_name           = iv_entity_name
                             iv_entity_set_name       = iv_entity_set_name
                             iv_source_name           = iv_source_name
                             it_key_tab               = it_key_tab
                             io_request_object_read   = io_request_object
                             io_tech_request_context_read = io_tech_request_context
                             it_navigation_path       = it_navigation_path
                             er_entity                = er_entity
                             es_response_context_read = es_response_context
                             mo_context               = mo_context
                             ) ).
    lo_entitat->read( ).
    DATA(lo_antwort) = CAST zif_rs_3dpc_antwort( lo_entitat ).
    lo_antwort->get_response_read( IMPORTING es_entity = er_entity
                                             es_response_context = es_response_context
                                             es_mo_context = mo_context ).

  ENDMETHOD.


  METHOD benutzerrollense_get_entityset.
*----------------------------------------------------------------------*
* SBB AG, SAP CCC-IT, Bern/Berne/Berna
*----------------------------------------------------------------------*
*
*-----------Änderungsdokumentation / documentation of changes----------*
* Version SAP-Rel Datum Bearbeiter
* Beschreibung Role Entity Set
*----------------------------------------------------------------------*
* >000< 7.50 28.10.2020 Firma(DXC Technology) / Name(Shernan Bamba)
* Programmerstellung / creation of program
*----------------------------------------------------------------------*
* >001< 7.50 tt.mm.jjjj Firma(company) / Name(name)
* ...
*----------------------------------------------------------------------*

  ENDMETHOD.


  METHOD katalogkategorie_get_entity.
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
    DATA(lo_entitat) = zcl_rs_3katalogkategorie=>create(
           VALUE zcl_rs_3katalogkategorie=>ts_input_parameters(
                             iv_entity_name           = iv_entity_name
                             iv_entity_set_name       = iv_entity_set_name
                             iv_source_name           = iv_source_name
                             it_key_tab               = it_key_tab
                             io_request_object_read   = io_request_object
                             io_tech_request_context_read = io_tech_request_context
                             it_navigation_path       = it_navigation_path
                             er_entity                = er_entity
                             es_response_context_read = es_response_context
                             mo_context = mo_context
                             ) ).
    lo_entitat->read( ).
    DATA(lo_antwort) = CAST zif_rs_3dpc_antwort( lo_entitat ).
    lo_antwort->get_response_read( IMPORTING es_entity = er_entity
                                             es_response_context = es_response_context
                                             es_mo_context = mo_context ).
  ENDMETHOD.


  METHOD katalogkategorie_get_entityset.
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
    DATA(lo_entitat) = zcl_rs_3katalogkategorie=>create(
            VALUE zcl_rs_3katalogkategorie=>ts_input_parameters(
                              iv_entity_name           = iv_entity_name
                              iv_entity_set_name       = iv_entity_set_name
                              iv_source_name           = iv_source_name
                              it_filter_select_options = it_filter_select_options
                              is_paging                = is_paging
                              it_key_tab               = it_key_tab
                              it_navigation_path       = it_navigation_path
                              it_order                 = it_order
                              iv_filter_string         = iv_filter_string
                              iv_search_string         = iv_search_string
                              io_tech_request_context_query  = io_tech_request_context
                              et_entityset             = et_entityset
                              es_response_context_set  = es_response_context
                              mo_context = mo_context
                              ) ).
    lo_entitat->query( ).
    DATA(lo_antwort) = CAST zif_rs_3dpc_antwort( lo_entitat ).
    lo_antwort->get_response_query( IMPORTING es_t_entity = et_entityset
                                            es_response_context = es_response_context
                                            es_mo_context = mo_context ).

  ENDMETHOD.


  METHOD katalogkomplexit_get_entity.
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
    DATA(lo_entitat) = zcl_rs_3katalogkomplexitat=>create(
           VALUE zcl_rs_3katalogkomplexitat=>ts_input_parameters(
                             iv_entity_name           = iv_entity_name
                             iv_entity_set_name       = iv_entity_set_name
                             iv_source_name           = iv_source_name
                             it_key_tab               = it_key_tab
                             io_request_object_read   = io_request_object
                             io_tech_request_context_read = io_tech_request_context
                             it_navigation_path       = it_navigation_path
                             er_entity                = er_entity
                             es_response_context_read = es_response_context
                             mo_context = mo_context
                             ) ).
    lo_entitat->read( ).
    DATA(lo_antwort) = CAST zif_rs_3dpc_antwort( lo_entitat ).
    lo_antwort->get_response_read( IMPORTING es_entity = er_entity
                                             es_response_context = es_response_context
                                             es_mo_context = mo_context ).
  ENDMETHOD.


 method KATALOGKOMPLEXIT_GET_ENTITYSET.
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
    DATA(lo_entitat) = zcl_rs_3katalogkomplexitat=>create(
            VALUE zcl_rs_3katalogkomplexitat=>ts_input_parameters(
                              iv_entity_name           = iv_entity_name
                              iv_entity_set_name       = iv_entity_set_name
                              iv_source_name           = iv_source_name
                              it_filter_select_options = it_filter_select_options
                              is_paging                = is_paging
                              it_key_tab               = it_key_tab
                              it_navigation_path       = it_navigation_path
                              it_order                 = it_order
                              iv_filter_string         = iv_filter_string
                              iv_search_string         = iv_search_string
                              io_tech_request_context_query  = io_tech_request_context
                              et_entityset             = et_entityset
                              es_response_context_set  = es_response_context
                              mo_context = mo_context
                              ) ).
    lo_entitat->query( ).
    DATA(lo_antwort) = CAST zif_rs_3dpc_antwort( lo_entitat ).
    lo_antwort->get_response_query( IMPORTING es_t_entity = et_entityset
                                          es_response_context = es_response_context
                                          es_mo_context = mo_context ).
  ENDMETHOD.


  METHOD kataloglieferobj_get_entity.
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
    DATA(lo_entitat) = zcl_rs_3kataloglieferobjekte=>create(
           VALUE zcl_rs_3kataloglieferobjekte=>ts_input_parameters(
                             iv_entity_name           = iv_entity_name
                             iv_entity_set_name       = iv_entity_set_name
                             iv_source_name           = iv_source_name
                             it_key_tab               = it_key_tab
                             io_request_object_read   = io_request_object
                             io_tech_request_context_read = io_tech_request_context
                             it_navigation_path       = it_navigation_path
                             er_entity                = er_entity
                             es_response_context_read = es_response_context
                             mo_context = mo_context
                             ) ).
    lo_entitat->read( ).
    DATA(lo_antwort) = CAST zif_rs_3dpc_antwort( lo_entitat ).
    lo_antwort->get_response_read( IMPORTING es_entity = er_entity
                                          es_response_context = es_response_context
                                          es_mo_context = mo_context ).
  ENDMETHOD.


  method KATALOGLIEFEROBJ_GET_ENTITYSET.
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
    DATA(lo_entitat) = zcl_rs_3kataloglieferobjekte=>create(
            VALUE zcl_rs_3kataloglieferobjekte=>ts_input_parameters(
                              iv_entity_name           = iv_entity_name
                              iv_entity_set_name       = iv_entity_set_name
                              iv_source_name           = iv_source_name
                              it_filter_select_options = it_filter_select_options
                              is_paging                = is_paging
                              it_key_tab               = it_key_tab
                              it_navigation_path       = it_navigation_path
                              it_order                 = it_order
                              iv_filter_string         = iv_filter_string
                              iv_search_string         = iv_search_string
                              io_tech_request_context_query  = io_tech_request_context
                              et_entityset             = et_entityset
                              es_response_context_set  = es_response_context
                              mo_context = mo_context
                              ) ).
    lo_entitat->query( ).
    DATA(lo_antwort) = CAST zif_rs_3dpc_antwort( lo_entitat ).
    lo_antwort->get_response_query( IMPORTING es_t_entity = et_entityset
                                          es_response_context = es_response_context
                                          es_mo_context = mo_context ).
  ENDMETHOD.


  METHOD katalogpreisset_get_entity.
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
    DATA(lo_entitat) = zcl_rs_3katalogpreis=>create(
               VALUE zcl_rs_3katalogpreis=>ts_input_parameters(
                                 iv_entity_name           = iv_entity_name
                                 iv_entity_set_name       = iv_entity_set_name
                                 iv_source_name           = iv_source_name
                                 it_key_tab               = it_key_tab
                                 io_request_object_read   = io_request_object
                                 io_tech_request_context_read = io_tech_request_context
                                 it_navigation_path       = it_navigation_path
                                 er_entity                = er_entity
                                 es_response_context_read = es_response_context
                                 ) ).
    lo_entitat->read( ).
    DATA(lo_antwort) = CAST zif_rs_3dpc_antwort( lo_entitat ).
    lo_antwort->get_response_read( IMPORTING es_entity = er_entity
                                             es_response_context = es_response_context ).
  ENDMETHOD.


  METHOD katalogpreisset_get_entityset.
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
    DATA(lo_entitat) = zcl_rs_3katalogpreis=>create(
                VALUE zcl_rs_3katalogpreis=>ts_input_parameters(
                                  iv_entity_name           = iv_entity_name
                                  iv_entity_set_name       = iv_entity_set_name
                                  iv_source_name           = iv_source_name
                                  it_filter_select_options = it_filter_select_options
                                  is_paging                = is_paging
                                  it_key_tab               = it_key_tab
                                  it_navigation_path       = it_navigation_path
                                  it_order                 = it_order
                                  iv_filter_string         = iv_filter_string
                                  iv_search_string         = iv_search_string
                                  io_tech_request_context_query  = io_tech_request_context
                                  et_entityset             = et_entityset
                                  es_response_context_set  = es_response_context
                                  ) ).
    lo_entitat->query( ).
    DATA(lo_antwort) = CAST zif_rs_3dpc_antwort( lo_entitat ).
    lo_antwort->get_response_query( IMPORTING es_t_entity = et_entityset
                                              es_response_context = es_response_context ).
  ENDMETHOD.


  METHOD mdartikelset_create_entity.
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
    DATA(lo_entitat) = zcl_rs_3mdartikel=>create(
           VALUE zcl_rs_3mdartikel=>ts_input_parameters(
                             iv_entity_name           = iv_entity_name
                             iv_entity_set_name       = iv_entity_set_name
                             iv_source_name           = iv_source_name
                             it_key_tab               = it_key_tab
                             io_tech_request_context_create = io_tech_request_context
                             it_navigation_path       = it_navigation_path
                             io_data_provider         = io_data_provider
                             er_entity                = er_entity
                             mo_context = mo_context
                             ) ).
    lo_entitat->post( ).
    DATA(lo_antwort) = CAST zif_rs_3dpc_antwort( lo_entitat ).
    lo_antwort->get_response_post( IMPORTING es_entity = er_entity
                                             es_mo_context = mo_context ).
  ENDMETHOD.


  METHOD mdartikelset_get_entity.
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
    DATA(lo_entitat) = zcl_rs_3mdartikel=>create(
              VALUE zcl_rs_3mdartikel=>ts_input_parameters(
                                iv_entity_name           = iv_entity_name
                                iv_entity_set_name       = iv_entity_set_name
                                iv_source_name           = iv_source_name
                                it_key_tab               = it_key_tab
                                io_request_object_read   = io_request_object
                                io_tech_request_context_read = io_tech_request_context
                                it_navigation_path       = it_navigation_path
                                er_entity                = er_entity
                                es_response_context_read = es_response_context
                                mo_context = mo_context
                                ) ).
    lo_entitat->read( ).
    DATA(lo_antwort) = CAST zif_rs_3dpc_antwort( lo_entitat ).
    lo_antwort->get_response_read( IMPORTING es_entity = er_entity
                                             es_response_context = es_response_context
                                             es_mo_context = mo_context ).
  ENDMETHOD.


  METHOD mdartikelset_get_entityset.
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
    DATA(lo_entitat) = zcl_rs_3mdartikel=>create(
           VALUE zcl_rs_3mdartikel=>ts_input_parameters(
                             iv_entity_name           = iv_entity_name
                             iv_entity_set_name       = iv_entity_set_name
                             iv_source_name           = iv_source_name
                             it_filter_select_options = it_filter_select_options
                             is_paging                = is_paging
                             it_key_tab               = it_key_tab
                             it_navigation_path       = it_navigation_path
                             it_order                 = it_order
                             iv_filter_string         = iv_filter_string
                             iv_search_string         = iv_search_string
                             io_tech_request_context_query  = io_tech_request_context
                             et_entityset             = et_entityset
                             es_response_context_set  = es_response_context
                             mo_context = mo_context
                             ) ).
    lo_entitat->query( ).
    DATA(lo_antwort) = CAST zif_rs_3dpc_antwort( lo_entitat ).
    lo_antwort->get_response_query( IMPORTING es_t_entity =      et_entityset
                                              es_response_context = es_response_context
                                              es_mo_context = mo_context ).
  ENDMETHOD.


  METHOD mdartikelset_update_entity.
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
    DATA(lo_entitat) = zcl_rs_3mdartikel=>create(
              VALUE zcl_rs_3mdartikel=>ts_input_parameters(
                                iv_entity_name           = iv_entity_name
                                iv_entity_set_name       = iv_entity_set_name
                                iv_source_name           = iv_source_name
                                it_key_tab               = it_key_tab
                                io_tech_request_context_update = io_tech_request_context
                                it_navigation_path       = it_navigation_path
                                io_data_provider         = io_data_provider
                                er_entity                = er_entity
                                mo_context = mo_context
                                ) ).
    lo_entitat->update( ).
    DATA(lo_antwort) = CAST zif_rs_3dpc_antwort( lo_entitat ).
    lo_antwort->get_response_update( IMPORTING es_entity = er_entity
                                               es_mo_context = mo_context ).
  ENDMETHOD.


  METHOD projektdropdownl_get_entityset.
*----------------------------------------------------------------------*
* SBB AG, SAP CCC-IT, Bern/Berne/Berna
*----------------------------------------------------------------------*
*
*-----------Änderungsdokumentation / documentation of changes----------*
* Version SAP-Rel Datum Bearbeiter
* Beschreibung Other Project Related Dropdown fields
*----------------------------------------------------------------------*
* >000< 7.50 28.10.2020 Firma(DXC Technology) / Name(Shernan Bamba)
* Programmerstellung / creation of program
*----------------------------------------------------------------------*
* >001< 7.50 tt.mm.jjjj Firma(company) / Name(name)
* ...
*----------------------------------------------------------------------*

    DATA(lo_entitat) = zcl_rs_3projektdropdown=>create(
              VALUE zcl_rs_3projektdropdown=>ts_input_parameters(
              iv_entity_name = iv_entity_name
              iv_entity_set_name = iv_entity_set_name
              iv_source_name = iv_source_name
              it_filter_select_options = it_filter_select_options
              is_paging = is_paging
              it_key_tab = it_key_tab
              it_navigation_path = it_navigation_path
              it_order = it_order
              iv_filter_string = iv_filter_string
              iv_search_string = iv_search_string
              io_tech_request_context_query = io_tech_request_context
              et_entityset = et_entityset
              mo_context = mo_context
              es_response_context_set = es_response_context
              ) ).

    lo_entitat->query( ).
    DATA(lo_antwort) = CAST zif_rs_3dpc_antwort( lo_entitat ).
    lo_antwort->get_response_query( IMPORTING es_t_entity = et_entityset
                                              es_response_context = es_response_context
                                              es_mo_context = mo_context ).

  ENDMETHOD.


 method REFERENZEINHEITW_GET_ENTITY.
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
    DATA(lo_entitat) = zcl_rs_3domain_einheit_wert=>create(
           VALUE zcl_rs_3domain_einheit_wert=>ts_input_parameters(
                             iv_entity_name           = iv_entity_name
                             iv_entity_set_name       = iv_entity_set_name
                             iv_source_name           = iv_source_name
                             it_key_tab               = it_key_tab
                             io_request_object_read   = io_request_object
                             io_tech_request_context_read = io_tech_request_context
                             it_navigation_path       = it_navigation_path
                             er_entity                = er_entity
                             es_response_context_read = es_response_context
                             mo_context = mo_context
                             ) ).
    lo_entitat->read( ).
    DATA(lo_antwort) = CAST zif_rs_3dpc_antwort( lo_entitat ).
    lo_antwort->get_response_read( IMPORTING es_entity = er_entity
                                          es_response_context = es_response_context
                                          es_mo_context = mo_context ).
  endmethod.


  METHOD referenzeinheitw_get_entityset.
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
    DATA(lo_entitat) = zcl_rs_3domain_einheit_wert=>create(
                VALUE zcl_rs_3domain_einheit_wert=>ts_input_parameters(
                                  iv_entity_name           = iv_entity_name
                                  iv_entity_set_name       = iv_entity_set_name
                                  iv_source_name           = iv_source_name
                                  it_filter_select_options = it_filter_select_options
                                  is_paging                = is_paging
                                  it_key_tab               = it_key_tab
                                  it_navigation_path       = it_navigation_path
                                  it_order                 = it_order
                                  iv_filter_string         = iv_filter_string
                                  iv_search_string         = iv_search_string
                                  io_tech_request_context_query  = io_tech_request_context
                                  et_entityset             = et_entityset
                                  es_response_context_set  = es_response_context
                                  mo_context = mo_context
                                  ) ).
    lo_entitat->query( ).
    DATA(lo_antwort) = CAST zif_rs_3dpc_antwort( lo_entitat ).
    lo_antwort->get_response_query( IMPORTING es_t_entity = et_entityset
                                          es_response_context = es_response_context
                                          es_mo_context = mo_context ).
  ENDMETHOD.


  method REFERENZEINZELVE_GET_ENTITY.
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
    DATA(lo_entitat) = zcl_rs_3domain_einzelvertrag=>create(
           VALUE zcl_rs_3domain_einzelvertrag=>ts_input_parameters(
                             iv_entity_name           = iv_entity_name
                             iv_entity_set_name       = iv_entity_set_name
                             iv_source_name           = iv_source_name
                             it_key_tab               = it_key_tab
                             io_request_object_read   = io_request_object
                             io_tech_request_context_read = io_tech_request_context
                             it_navigation_path       = it_navigation_path
                             er_entity                = er_entity
                             es_response_context_read = es_response_context
                             mo_context = mo_context
                             ) ).
    lo_entitat->read( ).
    DATA(lo_antwort) = CAST zif_rs_3dpc_antwort( lo_entitat ).
    lo_antwort->get_response_read( IMPORTING es_entity = er_entity
                                          es_response_context = es_response_context
                                          es_mo_context = mo_context ).
  endmethod.


  METHOD referenzeinzelve_get_entityset.
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
    DATA(lo_entitat) = zcl_rs_3domain_einzelvertrag=>create(
               VALUE zcl_rs_3domain_einzelvertrag=>ts_input_parameters(
                                 iv_entity_name           = iv_entity_name
                                 iv_entity_set_name       = iv_entity_set_name
                                 iv_source_name           = iv_source_name
                                 it_filter_select_options = it_filter_select_options
                                 is_paging                = is_paging
                                 it_key_tab               = it_key_tab
                                 it_navigation_path       = it_navigation_path
                                 it_order                 = it_order
                                 iv_filter_string         = iv_filter_string
                                 iv_search_string         = iv_search_string
                                 io_tech_request_context_query  = io_tech_request_context
                                 et_entityset             = et_entityset
                                 es_response_context_set  = es_response_context
                                 mo_context = mo_context
                                 ) ).
    lo_entitat->query( ).
    DATA(lo_antwort) = CAST zif_rs_3dpc_antwort( lo_entitat ).
    lo_antwort->get_response_query( IMPORTING es_t_entity = et_entityset
                                          es_response_context = es_response_context
                                          es_mo_context = mo_context ).
  ENDMETHOD.


  METHOD referenzprojecta_get_entity.
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
    DATA(lo_entitat) = zcl_rs_3prjabrstat=>create(
           VALUE zcl_rs_3prjabrstat=>ts_input_parameters(
                             iv_entity_name           = iv_entity_name
                             iv_entity_set_name       = iv_entity_set_name
                             iv_source_name           = iv_source_name
                             it_key_tab               = it_key_tab
                             io_request_object_read   = io_request_object
                             io_tech_request_context_read = io_tech_request_context
                             it_navigation_path       = it_navigation_path
                             er_entity                = er_entity
                             es_response_context_read = es_response_context
                             mo_context = mo_context
                             ) ).
    lo_entitat->read( ).
    DATA(lo_antwort) = CAST zif_rs_3dpc_antwort( lo_entitat ).
    lo_antwort->get_response_read( IMPORTING es_entity = er_entity
                                             es_response_context = es_response_context
                                             es_mo_context = mo_context ).
  ENDMETHOD.


  METHOD referenzprojecta_get_entityset.
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
    DATA(lo_entitat) = zcl_rs_3prjabrstat=>create(
                VALUE zcl_rs_3prjabrstat=>ts_input_parameters(
                                  iv_entity_name           = iv_entity_name
                                  iv_entity_set_name       = iv_entity_set_name
                                  iv_source_name           = iv_source_name
                                  it_filter_select_options = it_filter_select_options
                                  is_paging                = is_paging
                                  it_key_tab               = it_key_tab
                                  it_navigation_path       = it_navigation_path
                                  it_order                 = it_order
                                  iv_filter_string         = iv_filter_string
                                  iv_search_string         = iv_search_string
                                  io_tech_request_context_query  = io_tech_request_context
                                  et_entityset             = et_entityset
                                  es_response_context_set  = es_response_context
                                  mo_context = mo_context
                                  ) ).
    lo_entitat->query( ).
    DATA(lo_antwort) = CAST zif_rs_3dpc_antwort( lo_entitat ).
    lo_antwort->get_response_query( IMPORTING es_t_entity = et_entityset
                                              es_response_context = es_response_context
                                              es_mo_context = mo_context ).
  ENDMETHOD.


  METHOD suchebenutzerset_get_entityset.
*----------------------------------------------------------------------*
* SBB AG, SAP CCC-IT, Bern/Berne/Berna
*----------------------------------------------------------------------*
*
*-----------Änderungsdokumentation / documentation of changes----------*
* Version SAP-Rel Datum Bearbeiter
* Beschreibung Get EntitySet Usernames for UI5 Dropdown
*----------------------------------------------------------------------*
* >000< 7.50 28.10.2020 Firma(DXC Technology) / Name(Shernan Bamba)
* Programmerstellung / creation of program
*----------------------------------------------------------------------*
* >001< 7.50 tt.mm.jjjj Firma(company) / Name(name)
* ...
*----------------------------------------------------------------------*

    DATA(lo_entitat) = zcl_rs_3suchebenutzer=>create(
          VALUE zcl_rs_3suchebenutzer=>ts_input_parameters(
          iv_entity_name = iv_entity_name
          iv_entity_set_name = iv_entity_set_name
          iv_source_name = iv_source_name
          it_filter_select_options = it_filter_select_options
          is_paging = is_paging
          it_key_tab = it_key_tab
          it_navigation_path = it_navigation_path
          it_order = it_order
          iv_filter_string = iv_filter_string
          iv_search_string = iv_search_string
          io_tech_request_context_query = io_tech_request_context
          et_entityset = et_entityset
          mo_context = mo_context
          es_response_context_set = es_response_context
          ) ).

    lo_entitat->query( ).
    DATA(lo_antwort) = CAST zif_rs_3dpc_antwort( lo_entitat ).
    lo_antwort->get_response_query( IMPORTING es_t_entity = et_entityset
                                              es_response_context = es_response_context
                                              es_mo_context = mo_context ).

  ENDMETHOD.


  METHOD verrechnungselem_get_entityset.
*----------------------------------------------------------------------*
* SBB AG, SAP CCC-IT, Bern/Berne/Berna
*----------------------------------------------------------------------*
*
*-----------Änderungsdokumentation / documentation of changes----------*
* Version SAP-Rel Datum Bearbeiter
* Beschreibung Get accounting element
*----------------------------------------------------------------------*
* >000< 7.50 28.10.2020 Firma(DXC Technology) / Name(Shernan Bamba)
* Programmerstellung / creation of program
*----------------------------------------------------------------------*
* >001< 7.50 tt.mm.jjjj Firma(company) / Name(name)
* ...
*----------------------------------------------------------------------*

    DATA(lo_entitat) = zcl_rs_3verrechnungselemente=>create(
              VALUE zcl_rs_3verrechnungselemente=>ts_input_parameters(
              iv_entity_name = iv_entity_name
              iv_entity_set_name = iv_entity_set_name
              iv_source_name = iv_source_name
              it_filter_select_options = it_filter_select_options
              is_paging = is_paging
              it_key_tab = it_key_tab
              it_navigation_path = it_navigation_path
              it_order = it_order
              iv_filter_string = iv_filter_string
              iv_search_string = iv_search_string
              io_tech_request_context_query = io_tech_request_context
              et_entityset = et_entityset
              mo_context = mo_context
              es_response_context_set = es_response_context
              ) ).

    lo_entitat->query( ).
    DATA(lo_antwort) = CAST zif_rs_3dpc_antwort( lo_entitat ).
    lo_antwort->get_response_query( IMPORTING es_t_entity = et_entityset
                                              es_response_context = es_response_context
                                              es_mo_context = mo_context ).

  ENDMETHOD.


  METHOD vorhabenforecast_get_entity.
*----------------------------------------------------------------------*
* SBB AG, SAP CCC-IT, Bern/Berne/Berna
*----------------------------------------------------------------------*
*
*-----------Änderungsdokumentation / documentation of changes----------*
* Version SAP-Rel Datum Bearbeiter
* Beschreibung Read forecast values for a month
*----------------------------------------------------------------------*
* >000< 7.50 03.11.2020 Firma(DXC Technology) / Name(Leonardo Ramos)
* Programmerstellung / creation of program
*----------------------------------------------------------------------*
* >001< 7.50 tt.mm.jjjj Firma(company) / Name(name)
* ...
*----------------------------------------------------------------------*
    DATA(lo_entitat) = zcl_rs_3vorhabenforecast=>create(
                 VALUE zcl_rs_3vorhabenforecast=>ts_input_parameters(
                                   iv_entity_name           = iv_entity_name
                                   iv_entity_set_name       = iv_entity_set_name
                                   iv_source_name           = iv_source_name
                                   it_key_tab               = it_key_tab
                                   io_request_object_read   = io_request_object
                                   io_tech_request_context_read = io_tech_request_context
                                   it_navigation_path       = it_navigation_path
                                   er_entity                = er_entity
                                   mo_context               = mo_context
                                   es_response_context_read = es_response_context
                                   ) ).
    lo_entitat->read( ).
    DATA(lo_antwort) = CAST zif_rs_3dpc_antwort( lo_entitat ).
    lo_antwort->get_response_read( IMPORTING es_entity = er_entity
                                             es_response_context = es_response_context
                                             es_mo_context = mo_context ).

  ENDMETHOD.


  METHOD vorhabenforecast_get_entityset.
*----------------------------------------------------------------------*
* SBB AG, SAP CCC-IT, Bern/Berne/Berna
*----------------------------------------------------------------------*
*
*-----------Änderungsdokumentation / documentation of changes----------*
* Version SAP-Rel Datum Bearbeiter
* Beschreibung Read monthly forecasts of a project
*----------------------------------------------------------------------*
* >000< 7.50 03.11.2020 Firma(DXC Technology) / Name(Leonardo Ramos)
* Programmerstellung / creation of program
*----------------------------------------------------------------------*
* >001< 7.50 tt.mm.jjjj Firma(company) / Name(name)
* ...
*----------------------------------------------------------------------*
    DATA(lo_entitat) = zcl_rs_3vorhabenforecast=>create(
               VALUE zcl_rs_3vorhabenforecast=>ts_input_parameters(
                                 iv_entity_name           = iv_entity_name
                                 iv_entity_set_name       = iv_entity_set_name
                                 iv_source_name           = iv_source_name
                                 it_filter_select_options = it_filter_select_options
                                 is_paging                = is_paging
                                 it_key_tab               = it_key_tab
                                 it_navigation_path       = it_navigation_path
                                 it_order                 = it_order
                                 iv_filter_string         = iv_filter_string
                                 iv_search_string         = iv_search_string
                                 io_tech_request_context_query  = io_tech_request_context
                                 et_entityset             = et_entityset
                                 mo_context               = mo_context
                                 es_response_context_set  = es_response_context
                                 ) ).
    lo_entitat->query( ).
    DATA(lo_antwort) = CAST zif_rs_3dpc_antwort( lo_entitat ).
    lo_antwort->get_response_query( IMPORTING es_t_entity =      et_entityset
                                              es_response_context = es_response_context ).

  ENDMETHOD.


  method VORHABENFORECAST_UPDATE_ENTITY.
*----------------------------------------------------------------------*
* SBB AG, SAP CCC-IT, Bern/Berne/Berna
*----------------------------------------------------------------------*
*
*-----------Änderungsdokumentation / documentation of changes----------*
* Version SAP-Rel Datum Bearbeiter
* Beschreibung Update forecast values for a month
*----------------------------------------------------------------------*
* >000< 7.50 03.11.2020 Firma(DXC Technology) / Name(Leonardo Ramos)
* Programmerstellung / creation of program
*----------------------------------------------------------------------*
* >001< 7.50 tt.mm.jjjj Firma(company) / Name(name)
* ...
*----------------------------------------------------------------------*
    DATA(lo_entitat) = zcl_rs_3vorhabenforecast=>create(
               VALUE zcl_rs_3vorhabenforecast=>ts_input_parameters(
                                 iv_entity_name           = iv_entity_name
                                 iv_entity_set_name       = iv_entity_set_name
                                 iv_source_name           = iv_source_name
                                 it_key_tab               = it_key_tab
                                 io_tech_request_context_update = io_tech_request_context
                                 it_navigation_path       = it_navigation_path
                                 io_data_provider         = io_data_provider
                                 er_entity                = er_entity
                                 mo_context               = mo_context
                                 ) ).
    lo_entitat->update( ).
    DATA(lo_antwort) = CAST zif_rs_3dpc_antwort( lo_entitat ).
    lo_antwort->get_response_update( IMPORTING es_entity = er_entity
                                               es_mo_context = mo_context ).

  endmethod.


  METHOD vorhabenset_create_entity.
*----------------------------------------------------------------------*
* SBB AG, SAP CCC-IT, Bern/Berne/Berna
*----------------------------------------------------------------------*
*
*-----------Änderungsdokumentation / documentation of changes----------*
* Version SAP-Rel Datum Bearbeiter
* Beschreibung Create Project Entity class
*----------------------------------------------------------------------*
* >000< 7.50 28.10.2020 Firma(DXC Technology) / Name(Shernan Bamba)
* Programmerstellung / creation of program
*----------------------------------------------------------------------*
* >001< 7.50 tt.mm.jjjj Firma(company) / Name(name)
* ...
*----------------------------------------------------------------------*

    DATA(lo_entitat) = zcl_rs_3vorhaben=>create(
            VALUE zcl_rs_3vorhaben=>ts_input_parameters(
                              iv_entity_name           = iv_entity_name
                              iv_entity_set_name       = iv_entity_set_name
                              iv_source_name           = iv_source_name
                              it_key_tab               = it_key_tab
                              io_tech_request_context_create = io_tech_request_context
                              it_navigation_path       = it_navigation_path
                              io_data_provider         = io_data_provider
                              er_entity                = er_entity
                              mo_context               = mo_context
                              ) ).
    lo_entitat->post( ).
    DATA(lo_antwort) = CAST zif_rs_3dpc_antwort( lo_entitat ).
    lo_antwort->get_response_post( IMPORTING es_entity = er_entity
                                             es_mo_context = mo_context ).

  ENDMETHOD.


  METHOD vorhabenset_get_entity.
*----------------------------------------------------------------------*
* SBB AG, SAP CCC-IT, Bern/Berne/Berna
*----------------------------------------------------------------------*
*
*-----------Änderungsdokumentation / documentation of changes----------*
* Version SAP-Rel Datum Bearbeiter
* Beschreibung Get Project Details Entity class
*----------------------------------------------------------------------*
* >000< 7.50 28.10.2020 Firma(DXC Technology) / Name(Shernan Bamba)
* Programmerstellung / creation of program
*----------------------------------------------------------------------*
* >001< 7.50 tt.mm.jjjj Firma(company) / Name(name)
* ...
*----------------------------------------------------------------------*

    DATA(lo_entitat) = zcl_rs_3vorhaben=>create(
            VALUE zcl_rs_3vorhaben=>ts_input_parameters(
                   iv_entity_name           = iv_entity_name
                   iv_entity_set_name       = iv_entity_set_name
                   iv_source_name           = iv_source_name
                   it_key_tab               = it_key_tab
                   io_request_object_read   = io_request_object
                   io_tech_request_context_read = io_tech_request_context
                   it_navigation_path       = it_navigation_path
                   er_entity                = er_entity
                   mo_context               = mo_context
                   es_response_context_read = es_response_context
                   ) ).
    lo_entitat->read( ).
    DATA(lo_antwort) = CAST zif_rs_3dpc_antwort( lo_entitat ).
    lo_antwort->get_response_read( IMPORTING es_entity           = er_entity
                                             es_response_context = es_response_context
                                             es_mo_context = mo_context ).

  ENDMETHOD.


  METHOD vorhabenset_get_entityset.
*----------------------------------------------------------------------*
* SBB AG, SAP CCC-IT, Bern/Berne/Berna
*----------------------------------------------------------------------*
*
*-----------Änderungsdokumentation / documentation of changes----------*
* Version SAP-Rel Datum Bearbeiter
* Beschreibung Get all projects entityset
*----------------------------------------------------------------------*
* >000< 7.50 28.10.2020 Firma(DXC Technology) / Name(Shernan Bamba)
* Programmerstellung / creation of program
*----------------------------------------------------------------------*
* >001< 7.50 tt.mm.jjjj Firma(company) / Name(name)
* ...
*----------------------------------------------------------------------*

    DATA(lo_entitat) = zcl_rs_3vorhaben=>create(
            VALUE zcl_rs_3vorhaben=>ts_input_parameters(
            iv_entity_name = iv_entity_name
            iv_entity_set_name = iv_entity_set_name
            iv_source_name = iv_source_name
            it_filter_select_options = it_filter_select_options
            is_paging = is_paging
            it_key_tab = it_key_tab
            it_navigation_path = it_navigation_path
            it_order = it_order
            iv_filter_string = iv_filter_string
            iv_search_string = iv_search_string
            io_tech_request_context_query = io_tech_request_context
            et_entityset = et_entityset
            mo_context = mo_context
            es_response_context_set = es_response_context
            ) ).

    lo_entitat->query( ).
    DATA(lo_antwort) = CAST zif_rs_3dpc_antwort( lo_entitat ).
    lo_antwort->get_response_query( IMPORTING es_t_entity = et_entityset
                                              es_response_context = es_response_context ).

  ENDMETHOD.


  METHOD vorhabenset_update_entity.
*----------------------------------------------------------------------*
* SBB AG, SAP CCC-IT, Bern/Berne/Berna
*----------------------------------------------------------------------*
*
*-----------Änderungsdokumentation / documentation of changes----------*
* Version SAP-Rel Datum Bearbeiter
* Beschreibung Update Project Enity Class
*----------------------------------------------------------------------*
* >000< 7.50 28.10.2020 Firma(DXC Technology) / Name(Shernan Bamba)
* Programmerstellung / creation of program
*----------------------------------------------------------------------*
* >001< 7.50 tt.mm.jjjj Firma(company) / Name(name)
* ...
*----------------------------------------------------------------------*

    DATA(lo_entitat) = zcl_rs_3vorhaben=>create(
            VALUE zcl_rs_3vorhaben=>ts_input_parameters(
                              iv_entity_name           = iv_entity_name
                              iv_entity_set_name       = iv_entity_set_name
                              iv_source_name           = iv_source_name
                              it_key_tab               = it_key_tab
                              io_tech_request_context_update = io_tech_request_context
                              it_navigation_path       = it_navigation_path
                              io_data_provider         = io_data_provider
                              er_entity                = er_entity
                              mo_context               = mo_context
                              ) ).
    lo_entitat->update( ).
    DATA(lo_antwort) = CAST zif_rs_3dpc_antwort( lo_entitat ).
    lo_antwort->get_response_update( IMPORTING es_entity = er_entity
                                               es_mo_context = mo_context ).

  ENDMETHOD.
ENDCLASS.
