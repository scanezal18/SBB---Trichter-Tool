class ZCL_RS_3VORHABENFORECAST definition
  public
  final
  create public .

public section.

  interfaces ZIF_RS_3DPC_ANTWORT .
  interfaces ZIF_RS_3DPC_ENTITAT .

  types TS_VORHABENFORECAST type ZRS_3TT_PROJFCST .
  types:
    TT_VORHABENFORECAST TYPE STANDARD TABLE OF ts_vorhabenforecast WITH DEFAULT KEY .
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
        er_entity                      TYPE ts_vorhabenforecast,
        et_entityset                   TYPE tt_vorhabenforecast,
        mo_context                     TYPE REF TO /iwbep/if_mgw_context,
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
      !IS_PARAMS type TS_INPUT_PARAMETERS .
  class-methods CREATE_FORECAST
    importing
      !IS_PROJECT type ZRS_3TT_VORHABEN
    exporting
      !ET_RETURN type BAPIRET2_T .
protected section.
private section.

  data GS_PARAMS type TS_INPUT_PARAMETERS .
  constants MC_VERBINDER type CHAR2 value '=>' ##NO_TEXT.
  constants:
    BEGIN OF mc_error_opt,
        business  TYPE char1 VALUE 'B',
        technical TYPE char1 VALUE 'T',
      END OF mc_error_opt .

  methods CHECK_PROJ_HDR
    importing
      !IS_ENTITY type TS_INPUT_PARAMETERS-ER_ENTITY .
  methods UPDATE_DATA .
  methods CREATE_DATA
    raising
      /IWBEP/CX_MGW_TECH_EXCEPTION
      /IWBEP/CX_MGW_BUSI_EXCEPTION .
  methods GET_DATA .
  methods READ_DATA .
  methods RAISE_ERROR
    importing
      !IV_OPT type CHAR1 default MC_ERROR_OPT-BUSINESS
      !IV_MESSAGE type STRING optional
      !IV_METHODNAME type STRING optional
      !IV_TEXTID like IF_T100_MESSAGE=>T100KEY default /IWBEP/CX_MGW_BUSI_EXCEPTION=>BUSINESS_ERROR .
ENDCLASS.



CLASS ZCL_RS_3VORHABENFORECAST IMPLEMENTATION.


  METHOD check_proj_hdr.
*----------------------------------------------------------------------*
* SBB AG, SAP CCC-IT, Bern/Berne/Berna
*----------------------------------------------------------------------*
*
*-----------Änderungsdokumentation / documentation of changes----------*
* Version SAP-Rel Datum Bearbeiter
* Beschreibung Update total forecast values for the project header
*----------------------------------------------------------------------*
* >000< 7.50 03.11.2020 Firma(DXC Technology) / Name(Leonardo Ramos)
* Programmerstellung / creation of program
*----------------------------------------------------------------------*
* >001< 7.50 tt.mm.jjjj Firma(company) / Name(name)
* ...
*----------------------------------------------------------------------*
    DATA: lv_aw TYPE zrs_3tt_vorhaben-projekt_forecast_aw,
          lv_tp TYPE zrs_3tt_vorhaben-projekt_forecast_tp.
    DATA: lv_message TYPE string.

   "get project header entry
    SELECT SINGLE *
      FROM zrs_3tt_vorhaben
      INTO @DATA(ls_vorhaben)
      WHERE vorhabensnummer EQ @is_entity-vorhabensnummer.
    IF sy-subrc EQ 0.
     "select all forecast entries for project
      SELECT *
        FROM zrs_3tt_projfcst
        INTO TABLE @DATA(lt_forecast)
        WHERE vorhabensnummer EQ @is_entity-vorhabensnummer.
      IF sy-subrc EQ 0.
        CLEAR: lv_aw, lv_tp.
       "collect total aw and tp values
        LOOP AT lt_forecast INTO DATA(ls_forecast).
          lv_aw = lv_aw + ls_forecast-forecast_aw.
          lv_tp = lv_tp + ls_forecast-forecast_tp.
        ENDLOOP.

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

       "modify the proj header values everytime a forecast entry is modified
        MODIFY zrs_3tt_vorhaben FROM ls_vorhaben.
        IF sy-subrc EQ 0.
          COMMIT WORK.
        ELSE.
          MESSAGE i005(zrs_3) WITH ls_vorhaben-vorhabensnummer INTO lv_message.
          raise_error( iv_message = lv_message ).
        ENDIF.
      ENDIF.
    ENDIF.

  ENDMETHOD.


  method CONSTRUCTOR.
*----------------------------------------------------------------------*
* SBB AG, SAP CCC-IT, Bern/Berne/Berna
*----------------------------------------------------------------------*
*
*-----------Änderungsdokumentation / documentation of changes----------*
* Version SAP-Rel Datum Bearbeiter
* Beschreibung Constructor
*----------------------------------------------------------------------*
* >000< 7.50 03.11.2020 Firma(DXC Technology) / Name(Leonardo Ramos)
* Programmerstellung / creation of program
*----------------------------------------------------------------------*
* >001< 7.50 tt.mm.jjjj Firma(company) / Name(name)
* ...
*----------------------------------------------------------------------*
    gs_params = is_params.
  endmethod.


  method CREATE.
*----------------------------------------------------------------------*
* SBB AG, SAP CCC-IT, Bern/Berne/Berna
*----------------------------------------------------------------------*
*
*-----------Änderungsdokumentation / documentation of changes----------*
* Version SAP-Rel Datum Bearbeiter
* Beschreibung Create instance of vorhabenforecast utility class
*----------------------------------------------------------------------*
* >000< 7.50 03.11.2020 Firma(DXC Technology) / Name(Leonardo Ramos)
* Programmerstellung / creation of program
*----------------------------------------------------------------------*
* >001< 7.50 tt.mm.jjjj Firma(company) / Name(name)
* ...
*----------------------------------------------------------------------*
    DATA(lo_forecast) = NEW zcl_rs_3vorhabenforecast( is_params ).
    ro_entitat = lo_forecast.
  endmethod.


  METHOD create_data.
*----------------------------------------------------------------------*
* SBB AG, SAP CCC-IT, Bern/Berne/Berna
*----------------------------------------------------------------------*
*
*-----------Änderungsdokumentation / documentation of changes----------*
* Version SAP-Rel Datum Bearbeiter
* Beschreibung Create monthly forecast
*----------------------------------------------------------------------*
* >000< 7.50 tt.mm.jjjj Firma(DXC Technology) / Name(Leonardo Ramos)
* Programmerstellung / creation of program
*----------------------------------------------------------------------*
* >001< 7.50 tt.mm.jjjj Firma(company) / Name(name)
* ...
*----------------------------------------------------------------------*
*    DATA ls_entity LIKE gs_params-er_entity.
*    DATA lv_message TYPE string.
*
*    gs_params-io_data_provider->read_entry_data( IMPORTING es_data = ls_entity ).
*    gs_params-er_entity = ls_entity.
*
*    IF ls_entity IS INITIAL.
*      RETURN.
*    ENDIF.
*
*    MODIFY zrs_3tt_projfcst
*      FROM ls_entity.
*    IF sy-subrc EQ 0.
*      COMMIT WORK.
*    ELSE.
**      MESSAGE i034(zrs_3) INTO lv_message.
**      raise_error( is_message = lv_message ).
*    ENDIF.

  ENDMETHOD.


  METHOD create_forecast.
*----------------------------------------------------------------------*
* SBB AG, SAP CCC-IT, Bern/Berne/Berna
*----------------------------------------------------------------------*
*
*-----------Änderungsdokumentation / documentation of changes----------*
* Version SAP-Rel Datum Bearbeiter
* Beschreibung Create monthly forecasts based on project header
*              project duration
*----------------------------------------------------------------------*
* >000< 7.50 03.11.2020 Firma(DXC Technology) / Name(Leonardo Ramos)
* Programmerstellung / creation of program
*----------------------------------------------------------------------*
* >001< 7.50 tt.mm.jjjj Firma(company) / Name(name)
* ...
*----------------------------------------------------------------------*
    DATA: lv_months        TYPE vtbbewe-atage,
          lv_projekt_start TYPE vtbbewe-dbervon,
          lv_projekt_ende  TYPE vtbbewe-dbervon,
          lv_start_old     TYPE vtbbewe-dbervon,
          lv_end_old       TYPE vtbbewe-dbervon,
          lt_forecast      TYPE STANDARD TABLE OF zrs_3tt_projfcst,
          ls_forecast      TYPE zrs_3tt_projfcst,
          lv_message       TYPE string,
          lt_return        TYPE STANDARD TABLE OF bapiret2.
    FIELD-SYMBOLS:
          <ls_forecst> TYPE zrs_3tt_projfcst.

    DATA(ls_project) = is_project.

    IF ls_project IS INITIAL.
      RETURN.
    ENDIF.

    "get projekt start duration
    lv_projekt_start = ls_project-entwicklungsstart. "dev start
    "if development start is empty, use projekt start
    IF lv_projekt_start IS INITIAL.
      lv_projekt_start = ls_project-projekt_start.
    ENDIF.

    "get projekt end date
    lv_projekt_ende = ls_project-projekt_ende.
    "set days to 01 for both
    lv_projekt_start+6(2) = '01'.
    lv_projekt_ende+6(2) = '01'.
    DATA(lv_month) = lv_projekt_start+4(2).
    DATA(lv_year) = lv_projekt_start+0(4).

*    "for project duration updates, check if the change in
*    "project duration doesn't affect created forecasts
*    SELECT SINGLE *
*      FROM zrs_3tt_vorhaben
*      INTO @DATA(ls_projekt)
*      WHERE vorhabensnummer EQ @ls_project-vorhabensnummer.
*    IF sy-subrc EQ 0.
*      lv_start_old = ls_projekt-entwicklungsstart. "dev start
*      IF lv_start_old IS INITIAL.
*        lv_start_old = ls_projekt-projekt_start.
*      ENDIF.
*      lv_end_old = ls_projekt-projekt_ende.
*      lv_start_old+6(2) = '01'.
*      lv_end_old+6(2) = '01'.
*      IF ( lv_start_old EQ lv_projekt_start )
*        AND ( lv_end_old  EQ lv_projekt_ende ).
*        "just incase only the days are changed and not months,
*        "then no need to create new forecasts
*        RETURN.
*      ENDIF.
*    ENDIF.

    "get number of months between start and end date
    CALL FUNCTION 'FIMA_DAYS_AND_MONTHS_AND_YEARS'
      EXPORTING
        i_date_from = lv_projekt_start
        i_date_to   = lv_projekt_ende
      IMPORTING
        e_months    = lv_months.

    "append first month forecast using start date
    lt_forecast = VALUE #( ( mandt = sy-mandt
                                   vorhabensnummer = ls_project-vorhabensnummer
                                   kalender_jahr_monat = lv_projekt_start
                                   forecast_aw = 0
                                   einheit_forecast_aw = zif_rs_3constants=>gc_einheit_forecast_aw
                                   forecast_tp = 0
                                   einheit_forecast_tp = zif_rs_3constants=>gc_einheit_forecast_tp ) ).

    IF lv_months IS NOT INITIAL.
      CLEAR: ls_forecast.
      "create remaining months up to end date
      DO lv_months TIMES.
        lv_month = lv_month + 1.
        "if month is more than 12(december), reset to 1(january) and add 1 to year
        IF lv_month EQ '13'.
          lv_month = '01'.
          lv_year = lv_projekt_start+0(4).
          lv_year = lv_year + 1.
        ENDIF.
        lv_projekt_start+4(2)	= lv_month.
        lv_projekt_start+0(4) = lv_year.
        ls_forecast-mandt = sy-mandt.
        ls_forecast-vorhabensnummer = ls_project-vorhabensnummer.
        ls_forecast-kalender_jahr_monat = lv_projekt_start.
        ls_forecast-forecast_aw = 0.
        ls_forecast-einheit_forecast_aw = zif_rs_3constants=>gc_einheit_forecast_aw.
        ls_forecast-forecast_tp = 0.
        ls_forecast-einheit_forecast_tp = zif_rs_3constants=>gc_einheit_forecast_tp.
        APPEND ls_forecast TO lt_forecast.
        CLEAR: ls_forecast.
        "make sure that the loop will be exited
        IF lv_projekt_start EQ lv_projekt_ende.
          EXIT.
        ENDIF.
      ENDDO.

    ENDIF.

    SORT lt_forecast BY vorhabensnummer kalender_jahr_monat.

    DELETE ADJACENT DUPLICATES FROM lt_forecast
      COMPARING ALL FIELDS.

    "check first for existing entries in forecast table incase
    "of change in project header dates
    SELECT *
      FROM zrs_3tt_projfcst
      INTO TABLE @DATA(lt_projfcst)
      WHERE vorhabensnummer EQ @ls_project-vorhabensnummer.
    IF sy-subrc EQ 0.
      UNASSIGN: <ls_forecst>.
      "copy old monthly forecasts values to new ones
      LOOP AT lt_forecast ASSIGNING <ls_forecst>.
        READ TABLE lt_projfcst INTO DATA(ls_projfcst)
          WITH KEY vorhabensnummer = <ls_forecst>-vorhabensnummer
                   kalender_jahr_monat = <ls_forecst>-kalender_jahr_monat.
        IF sy-subrc EQ 0.
          <ls_forecst>-forecast_aw = ls_projfcst-forecast_aw.
          <ls_forecst>-forecast_tp = ls_projfcst-forecast_tp.
        ENDIF.
      ENDLOOP.
      DELETE FROM zrs_3tt_projfcst
        WHERE vorhabensnummer EQ ls_project-vorhabensnummer.
      IF sy-subrc EQ 0.
        COMMIT WORK.
      ENDIF.
    ENDIF.

    "insert forecast values into forecast table
    INSERT zrs_3tt_projfcst
      FROM TABLE lt_forecast.
    IF sy-subrc EQ 0.
      COMMIT WORK.
    ELSE.
*      MESSAGE e035(zrs_3) INTO lv_message.
*      CALL METHOD raise_error( EXPORTING is_message = lv_message ).
      lt_return = VALUE #( (
                    type = zif_rs_3constants=>gc_message_type_warning
                    id = zif_rs_3constants=>gc_message_class
                    number = '040' "forecast creation error
                    message = ls_project-vorhabensnummer
                    ) ).
    ENDIF.

  ENDMETHOD.


  METHOD get_data.
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
    DATA(lv_osql_where_clause) = gs_params-io_tech_request_context_query->get_osql_where_clause( ).

    DATA: lt_entity TYPE STANDARD TABLE OF zrs_3tt_projfcst.
    DATA: ls_entity TYPE zrs_3tt_projfcst.

   "select all forecast for the project
    SELECT *
      FROM zrs_3tt_projfcst
      INTO TABLE @DATA(lt_forecast)
      WHERE (lv_osql_where_clause).
    IF sy-subrc EQ 0.
      DATA(lv_datum) = sy-datum.
      lv_datum+6(2) = '01'.
      SORT lt_forecast BY vorhabensnummer kalender_jahr_monat.
      DELETE lt_forecast WHERE kalender_jahr_monat LT lv_datum.
      gs_params-et_entityset = lt_forecast.
    ENDIF.

    IF gs_params-et_entityset[] IS NOT INITIAL.
      RETURN.
    ENDIF.


  ENDMETHOD.


  method RAISE_ERROR.
*----------------------------------------------------------------------*
* SBB AG, SAP CCC-IT, Bern/Berne/Berna
*----------------------------------------------------------------------*
*
*-----------Änderungsdokumentation / documentation of changes----------*
* Version SAP-Rel Datum Bearbeiter
* Beschreibung Raise error
*----------------------------------------------------------------------*
* >000< 7.50 03.11.2020 Firma(DXC Technology) / Name(Leonardo Ramos)
* Programmerstellung / creation of program
*----------------------------------------------------------------------*
* >001< 7.50 tt.mm.jjjj Firma(company) / Name(name)
* ...
*----------------------------------------------------------------------*
    DATA(lv_methodname) = iv_methodname.
    DATA(lv_message) = CONV bapi_msg( iv_message ).

    CASE iv_opt.
      WHEN mc_error_opt-business.
        IF lv_message IS NOT INITIAL.
          RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
            EXPORTING
              textid  = iv_textid
              message = lv_message.
        ENDIF.
      WHEN mc_error_opt-technical.
        IF lv_methodname IS NOT INITIAL.
          RAISE EXCEPTION TYPE /iwbep/cx_mgw_tech_exception
            EXPORTING
              textid = iv_textid
              method = lv_methodname.
        ENDIF.
      WHEN OTHERS.
        RETURN.
    ENDCASE.

  endmethod.


  method READ_DATA.
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
    DATA(lt_keys) = gs_params-io_tech_request_context_read->get_keys( ).

    IF lt_keys[] IS INITIAL.
      RETURN.
    ENDIF.

    DATA(lv_vorhabensnummer) = lt_keys[ name = 'VORHABENSNUMMER' ]-value.
    DATA(lv_monat) = lt_keys[ name = 'KALENDER_JAHR_MONAT' ]-value.

    SELECT SINGLE *
      FROM zrs_3tt_projfcst
      INTO @DATA(ls_entity)
        WHERE vorhabensnummer EQ @lv_vorhabensnummer
          AND kalender_jahr_monat EQ @lv_monat.
    IF sy-subrc EQ 0.
      MOVE-CORRESPONDING ls_entity TO gs_params-er_entity.
    ENDIF.

    IF gs_params-er_entity IS NOT INITIAL.
      RETURN.
    ENDIF.

  endmethod.


  METHOD update_data.
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

    DATA ls_entity LIKE gs_params-er_entity.
    DATA: lv_message TYPE string.

    gs_params-io_data_provider->read_entry_data( IMPORTING es_data = ls_entity ).
    gs_params-er_entity = ls_entity.

    IF ls_entity IS INITIAL.
      RETURN.
    ENDIF.

    ls_entity-mandt = sy-mandt.

   "update project forecast table
    MODIFY zrs_3tt_projfcst
      FROM ls_entity.
    IF sy-subrc EQ 0.
      COMMIT WORK.
    ELSE.
      MESSAGE i034(zrs_3) INTO lv_message.
      raise_error( iv_message = lv_message ).
    ENDIF.

   "update proj header total aw and tp forecast values
    check_proj_hdr( EXPORTING is_entity = ls_entity ).

  ENDMETHOD.


  METHOD zif_rs_3dpc_antwort~get_response_post.
*----------------------------------------------------------------------*
* SBB AG, SAP CCC-IT, Bern/Berne/Berna
*----------------------------------------------------------------------*
*
*-----------Änderungsdokumentation / documentation of changes----------*
* Version SAP-Rel Datum Bearbeiter
* Beschreibung Get response from post
*----------------------------------------------------------------------*
* >000< 7.50 tt.mm.jjjj Firma(DXC Technology) / Name(Leonardo Ramos)
* Programmerstellung / creation of program
*----------------------------------------------------------------------*
* >001< 7.50 tt.mm.jjjj Firma(company) / Name(name)
* ...
*----------------------------------------------------------------------*
    es_entity = gs_params-er_entity.

  ENDMETHOD.


  METHOD zif_rs_3dpc_antwort~get_response_query.
*----------------------------------------------------------------------*
* SBB AG, SAP CCC-IT, Bern/Berne/Berna
*----------------------------------------------------------------------*
*
*-----------Änderungsdokumentation / documentation of changes----------*
* Version SAP-Rel Datum Bearbeiter
* Beschreibung Get response from query
*----------------------------------------------------------------------*
* >000< 7.50 03.11.2020 Firma(DXC Technology) / Name(Leonardo Ramos)
* Programmerstellung / creation of program
*----------------------------------------------------------------------*
* >001< 7.50 tt.mm.jjjj Firma(company) / Name(name)
* ...
*----------------------------------------------------------------------*
    es_t_entity = gs_params-et_entityset.
    es_response_context = gs_params-es_response_context_set.
    es_mo_context = gs_params-mo_context.

  ENDMETHOD.


  METHOD zif_rs_3dpc_antwort~get_response_read.
*----------------------------------------------------------------------*
* SBB AG, SAP CCC-IT, Bern/Berne/Berna
*----------------------------------------------------------------------*
*
*-----------Änderungsdokumentation / documentation of changes----------*
* Version SAP-Rel Datum Bearbeiter
* Beschreibung Get response from read
*----------------------------------------------------------------------*
* >000< 7.50 03.11.2020 Firma(DXC Technology) / Name(Leonardo Ramos)
* Programmerstellung / creation of program
*----------------------------------------------------------------------*
* >001< 7.50 tt.mm.jjjj Firma(company) / Name(name)
* ...
*----------------------------------------------------------------------*
    es_entity = gs_params-er_entity.
    es_response_context = gs_params-es_response_context_read.
    es_mo_context = gs_params-mo_context.

  ENDMETHOD.


  METHOD zif_rs_3dpc_antwort~get_response_update.
*----------------------------------------------------------------------*
* SBB AG, SAP CCC-IT, Bern/Berne/Berna
*----------------------------------------------------------------------*
*
*-----------Änderungsdokumentation / documentation of changes----------*
* Version SAP-Rel Datum Bearbeiter
* Beschreibung Get response from update
*----------------------------------------------------------------------*
* >000< 7.50 03.11.2020 Firma(DXC Technology) / Name(Leonardo Ramos)
* Programmerstellung / creation of program
*----------------------------------------------------------------------*
* >001< 7.50 tt.mm.jjjj Firma(company) / Name(name)
* ...
*----------------------------------------------------------------------*
    es_entity = gs_params-er_entity.
    es_mo_context = gs_params-mo_context.

  ENDMETHOD.


  method ZIF_RS_3DPC_ENTITAT~DELETE.
  endmethod.


  METHOD zif_rs_3dpc_entitat~post.
*----------------------------------------------------------------------*
* SBB AG, SAP CCC-IT, Bern/Berne/Berna
*----------------------------------------------------------------------*
*
*-----------Änderungsdokumentation / documentation of changes----------*
* Version SAP-Rel Datum Bearbeiter
* Beschreibung Get user list for project
*----------------------------------------------------------------------*
* >000< 7.50 tt.mm.jjjj Firma(DXC Technology) / Name(Leonardo Ramos)
* Programmerstellung / creation of program
*----------------------------------------------------------------------*
* >001< 7.50 tt.mm.jjjj Firma(company) / Name(name)
* ...
*----------------------------------------------------------------------*

    create_data( ).
    DATA(lo_response) = NEW zcl_rs_3vorhabenforecast( gs_params ).
    rv_if_response = lo_response.

  ENDMETHOD.


  METHOD zif_rs_3dpc_entitat~query.
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

    get_data( ).
    DATA(lo_response) = NEW zcl_rs_3vorhabenforecast( gs_params ).
    rv_if_response = lo_response.

  ENDMETHOD.


  method ZIF_RS_3DPC_ENTITAT~READ.
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

    read_data( ).
    DATA(lo_response) = NEW zcl_rs_3vorhabenforecast( gs_params ).
    rv_if_response = lo_response.

  endmethod.


  METHOD zif_rs_3dpc_entitat~update.
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

    update_data( ).
    DATA(lo_response) = NEW zcl_rs_3vorhabenforecast( gs_params ).
    rv_if_response = lo_response.

  ENDMETHOD.
ENDCLASS.
