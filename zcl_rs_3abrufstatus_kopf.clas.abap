class ZCL_RS_3ABRUFSTATUS_KOPF definition
  public
  final
  create public .

public section.

  interfaces ZIF_RS_3DPC_ABRUFSTATUS .

  class-methods CREATE
    importing
      !IS_PARAMS type ZIF_RS_3DPC_ABRUFSTATUS=>TS_INPUT_PARAMETERS
    returning
      value(RV_OBJECT) type ref to ZIF_RS_3DPC_ABRUFSTATUS
    raising
      /IWBEP/CX_MGW_TECH_EXCEPTION
      /IWBEP/CX_MGW_BUSI_EXCEPTION .
  methods CONSTRUCTOR
    importing
      !IS_PARAMS type ZIF_RS_3DPC_ABRUFSTATUS=>TS_INPUT_PARAMETERS
    raising
      /IWBEP/CX_MGW_TECH_EXCEPTION
      /IWBEP/CX_MGW_BUSI_EXCEPTION .
  PROTECTED SECTION.
private section.

  constants:
    BEGIN OF mc_error_opt,
      business  TYPE char1 VALUE 'B',
      technical TYPE char1 VALUE 'T',
    END OF mc_error_opt .
  data GS_PARAMS type ZIF_RS_3DPC_ABRUFSTATUS=>TS_INPUT_PARAMETERS .

  methods RAISE_ERROR
    importing
      !IS_OPT type CHAR1 default MC_ERROR_OPT-BUSINESS
      !IS_TEXTID like IF_T100_MESSAGE=>T100KEY default /IWBEP/CX_MGW_BUSI_EXCEPTION=>BUSINESS_ERROR
      !IS_MESSAGE type STRING optional
      !IS_METHODNAME type STRING optional
    raising
      /IWBEP/CX_MGW_TECH_EXCEPTION
      /IWBEP/CX_MGW_BUSI_EXCEPTION .
  methods CHECK_CREATE
    raising
      /IWBEP/CX_MGW_TECH_EXCEPTION
      /IWBEP/CX_MGW_BUSI_EXCEPTION .
  methods CHECK_UPDATE
    raising
      /IWBEP/CX_MGW_TECH_EXCEPTION
      /IWBEP/CX_MGW_BUSI_EXCEPTION .
ENDCLASS.



CLASS ZCL_RS_3ABRUFSTATUS_KOPF IMPLEMENTATION.


  METHOD check_create.
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
    DATA(ls_entity) = gs_params-er_entity_kopf.
    DATA(lv_uname) = sy-uname.
    DATA(lv_mandt) = sy-mandt.
    DATA(lv_langu) = sy-langu.
    DATA(lv_datetoday) = sy-datum.
    IF ls_entity IS INITIAL.
      RETURN.
    ENDIF.

    DATA ls_input_params TYPE zcl_rs_3abrufkopf=>ts_input_parameters.
    DATA(lo_abrufkopf) = NEW zcl_rs_3abrufkopf( ls_input_params ).

    ls_entity-mandt = lv_mandt.

    IF ls_entity-vorhabensnummer IS INITIAL.
      MESSAGE i044(zrs_3) INTO DATA(lv_message).
      raise_error( is_message = lv_message ). "Project Number must not be empty
    ELSE.
      DATA(lv_osql_where_clause) = lo_abrufkopf->mc_filter_values-to_read_vorhaben.
      REPLACE ALL OCCURRENCES: OF lo_abrufkopf->mc_filter_values-ampersand IN lv_osql_where_clause WITH ls_entity-vorhabensnummer,
                               OF lo_abrufkopf->mc_filter_values-at_sign IN lv_osql_where_clause WITH zif_rs_3constants=>gc_vorhaben_status_active.
      CONDENSE lv_osql_where_clause.
      DATA(lo_vorhaben) = NEW zcl_rs_3vorhaben( ).
      lo_vorhaben->get_all_projects( EXPORTING iv_condition = lv_osql_where_clause
                                     IMPORTING et_vorhaben = DATA(lt_vorhaben) ).
      IF lt_vorhaben IS INITIAL.
        MESSAGE i045(zrs_3) INTO lv_message.
        raise_error( is_message = lv_message ). " Project is not active
      ELSE.
        DATA(lt_entity) = lo_abrufkopf->get_db_values( lv_osql_where_clause ).
        IF lt_entity IS NOT INITIAL.
          gs_params-er_entity_kopf_old = lt_entity[ 1 ].
        ENDIF.
      ENDIF.
    ENDIF.

    IF ls_entity-status_abruf IS NOT INITIAL.
      DATA(lv_statusabruf_desc) = lo_abrufkopf->get_statusabruf_descr( ls_entity-status_abruf ).
      IF lv_statusabruf_desc IS INITIAL.
        MESSAGE i046(zrs_3) INTO lv_message.
        raise_error( is_message = lv_message )." Abruf Status does not exists
      ENDIF.
    ELSE.
      MESSAGE i046(zrs_3) INTO lv_message.
      raise_error( is_message = lv_message ). " Abruf Status does not exists
    ENDIF.

    IF ls_entity-abgerufen_durch IS INITIAL.
      ls_entity-abgerufen_durch = lv_uname.
    ENDIF.

    IF ls_entity-abgerufen_am IS INITIAL.
      ls_entity-abgerufen_am = lv_datetoday.
    ENDIF.

    IF ls_entity-einzelvertrag IS NOT INITIAL.
      DATA(lv_contract_descr) = lo_abrufkopf->get_individual_contract_descr( ls_entity-einzelvertrag ).
      CONDENSE lv_contract_descr.
      IF lv_contract_descr IS INITIAL.
        MESSAGE i047(zrs_3) INTO lv_message.
        raise_error( is_message = lv_message ). "Individual contract invalid
      ENDIF.
    ELSE.
      MESSAGE i048(zrs_3) INTO lv_message.
      raise_error( is_message = lv_message ). "Individual Contract must not be empty
    ENDIF.

    IF ls_entity-typ_verrechnungselement IS NOT INITIAL.
      DATA(lv_type_descr) = lo_abrufkopf->get_typverrechnungs_descr( ls_entity-typ_verrechnungselement ).
      CONDENSE lv_type_descr.
      IF lv_type_descr IS INITIAL.
        MESSAGE i049(zrs_3) INTO lv_message.
        raise_error( is_message = lv_message ). " account element type invalid.
      ENDIF.
    ENDIF.

    IF ls_entity-verrechnungselement IS NOT INITIAL.
      DATA(lv_kont_descr) = lo_abrufkopf->get_verrechnungselement_descr(
                                  is_value = ls_entity-verrechnungselement
                                  is_type = ls_entity-typ_verrechnungselement ).
      CONDENSE lv_kont_descr.
      IF lv_kont_descr IS INITIAL.
        MESSAGE i050(zrs_3) INTO lv_message.
        raise_error( is_message = lv_message ). "Account Element invalid
      ENDIF.
    ENDIF.

    CLEAR ls_entity-abrufnummer.
    ls_entity-abrufnummer = lo_abrufkopf->generate_abruf_nummer( ).

    IF ls_entity IS NOT INITIAL.
      CLEAR gs_params-er_entity_kopf.
      gs_params-er_entity_kopf = ls_entity.
    ENDIF.
  ENDMETHOD.


  METHOD check_update.
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
    DATA(ls_entity) = gs_params-er_entity_kopf.
    DATA(lv_uname) = sy-uname.
    DATA(lv_mandt) = sy-mandt.
    DATA(lv_langu) = sy-langu.
    DATA(lv_datetoday) = sy-datum.
    IF ls_entity IS INITIAL.
      RETURN.
    ENDIF.
    DATA ls_input_params TYPE zcl_rs_3abrufkopf=>ts_input_parameters.
    DATA(lo_abrufkopf) = NEW zcl_rs_3abrufkopf( ls_input_params ).

    ls_entity-mandt = lv_mandt.
    IF ls_entity-abrufnummer IS NOT INITIAL.
      DATA(lv_osql_where_clause) = lo_abrufkopf->mc_filter_values-abrufnummer_filter.
      REPLACE ALL OCCURRENCES: OF lo_abrufkopf->mc_filter_values-ampersand IN lv_osql_where_clause WITH ls_entity-abrufnummer.
      CONDENSE lv_osql_where_clause.
      DATA(lt_entity) = lo_abrufkopf->get_db_values( lv_osql_where_clause ).
      IF lt_entity IS INITIAL.
        MESSAGE i051(zrs_3) INTO DATA(lv_message).
        raise_error( is_message = lv_message ). "Abruf Nummer does not exists
      ELSE.
        gs_params-er_entity_kopf_old = lt_entity[ 1 ].
      ENDIF.
    ELSE.
      MESSAGE i051(zrs_3) INTO lv_message.
      raise_error( is_message = lv_message ). " Abruf Nummer does not exists
    ENDIF.

*    IF gs_params-er_entity_kopf_old IS NOT INITIAL.
*      IF gs_params-er_entity_kopf_old-lastchangedon <> ls_entity-lastchangedon.
*        raise_error( is_message = 'Daten werden derzeit geändert. Bitte versuchen Sie es später noch einmal.' ). "Data is currently being modified. PLease try again later.
*      ENDIF.
*    ENDIF.

    IF ls_entity-vorhabensnummer IS INITIAL.
      MESSAGE i044(zrs_3) INTO lv_message.
      raise_error( is_message = lv_message ). "Project Number must not be empty
    ELSE.
      IF lt_entity IS INITIAL.
        lv_osql_where_clause = lo_abrufkopf->mc_filter_values-to_read_vorhaben.
        REPLACE ALL OCCURRENCES: OF lo_abrufkopf->mc_filter_values-ampersand IN lv_osql_where_clause WITH ls_entity-vorhabensnummer,
                                 OF lo_abrufkopf->mc_filter_values-at_sign IN lv_osql_where_clause WITH zif_rs_3constants=>gc_vorhaben_status_active.
        CONDENSE lv_osql_where_clause.
        lt_entity = lo_abrufkopf->get_db_values( lv_osql_where_clause ).
        IF lt_entity IS INITIAL.
          MESSAGE i045(zrs_3) INTO lv_message.
          raise_error( is_message = lv_message ). "Project is not active
        ELSE.
          gs_params-er_entity_kopf_old = lt_entity[ 1 ].
        ENDIF.
      ELSE.
        IF NOT line_exists( lt_entity[ status = zif_rs_3constants=>gc_vorhaben_status_active ] ).
          MESSAGE i045(zrs_3) INTO lv_message.
          raise_error( is_message = lv_message )."Project is not active
        ENDIF.
      ENDIF.
    ENDIF.

    IF ls_entity-abgerufen_durch IS INITIAL.
      ls_entity-abgerufen_durch = lv_uname.
    ENDIF.

    IF ls_entity-abgerufen_am IS INITIAL.
      ls_entity-abgerufen_am = lv_datetoday.
    ENDIF.

    IF ls_entity-einzelvertrag IS NOT INITIAL.
      DATA(lv_contract_descr) = lo_abrufkopf->get_individual_contract_descr( ls_entity-einzelvertrag ).
      CONDENSE lv_contract_descr.
      IF lv_contract_descr IS INITIAL.
        MESSAGE i047(zrs_3) INTO lv_message.
        raise_error( is_message = lv_message ). "Individual contract invalid
      ENDIF.
    ELSE.
      MESSAGE i048(zrs_3) INTO lv_message.
      raise_error( is_message = lv_message ). "Individual Contract must not be empty
    ENDIF.

    IF ls_entity-status_abruf IS NOT INITIAL.
      DATA(lv_statusabruf_desc) = lo_abrufkopf->get_statusabruf_descr( ls_entity-status_abruf ).
      IF lv_statusabruf_desc IS INITIAL.
        MESSAGE i051(zrs_3) INTO lv_message.
        raise_error( is_message = lv_message ). "Abruf Nummer does not exists
      ENDIF.
    ELSE.
      MESSAGE i051(zrs_3) INTO lv_message.
      raise_error( is_message = lv_message ). "Abruf Nummer does not exists
    ENDIF.

    IF ls_entity-typ_verrechnungselement IS NOT INITIAL.
      DATA(lv_type_descr) = lo_abrufkopf->get_typverrechnungs_descr( ls_entity-typ_verrechnungselement ).
      CONDENSE lv_type_descr.
      IF lv_type_descr IS INITIAL.
        MESSAGE i049(zrs_3) INTO lv_message.
      raise_error( is_message = lv_message ).
      ENDIF.
    ENDIF.

    IF ls_entity-verrechnungselement IS NOT INITIAL.
      DATA(lv_kont_descr) = lo_abrufkopf->get_verrechnungselement_descr(
      is_value = ls_entity-verrechnungselement
      is_type = ls_entity-typ_verrechnungselement ).
      CONDENSE lv_kont_descr.
      IF lv_kont_descr IS INITIAL.
        MESSAGE i050(zrs_3) INTO lv_message.
        raise_error( is_message = lv_message ). "Account Element invalid
      ENDIF.
    ENDIF.

    IF ls_entity IS NOT INITIAL.
       CLEAR gs_params-er_entity_kopf.
       gs_params-er_entity_kopf = ls_entity.
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
    DATA(lo_status) = NEW zcl_rs_3abrufstatus_kopf( is_params ).
    rv_object = lo_status.
  ENDMETHOD.


  method RAISE_ERROR.
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


  METHOD zif_rs_3dpc_abrufstatus~check.
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
    DATA(lv_uname) = sy-uname.
    IF gs_params-er_entity_kopf IS INITIAL.
      RETURN.
    ENDIF.

    CASE gs_params-is_cud_type.
      WHEN zif_rs_3constants=>gc_cud_types-create.
            check_create( ).
      WHEN zif_rs_3constants=>gc_cud_types-update.
            check_update( ).
      WHEN OTHERS.
            CLEAR gs_params-er_entity_kopf.
    ENDCASE.

*   set timestamp
    DATA lv_timestamp TYPE timestamp.
    GET TIME STAMP FIELD lv_timestamp.
    DATA(lv_secs) = zif_rs_3constants=>gc_session_expires_secs-header * zif_rs_3constants=>gc_session_expires_secs-multiplier.

    lv_timestamp = cl_abap_tstmp=>subtractsecs( tstmp  = lv_timestamp
                                                secs = lv_secs ).
*    set changedlogs
    gs_params-er_entity_kopf-lastchangedby = lv_uname.
    gs_params-er_entity_kopf-lastchangedon = lv_timestamp.

  ENDMETHOD.


  method ZIF_RS_3DPC_ABRUFSTATUS~GET_RESULTS.
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
    es_entity_kopf = gs_params-er_entity_kopf.
    es_entity_pos = gs_params-er_entity_pos.
    es_entity_billing = gs_params-er_entity_billing.
    es_entity_kopf_old =  gs_params-er_entity_kopf_old.
    es_entity_pos_old =  gs_params-er_entity_pos_old.
    es_email_params = gs_params-es_email_params.
    es_entity_vorhaben = gs_params-er_entity_vorhaben.
    es_mo_context = gs_params-es_mo_context.
  endmethod.
ENDCLASS.
