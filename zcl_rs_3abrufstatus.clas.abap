class ZCL_RS_3ABRUFSTATUS definition
  public
  final
  create public

  global friends ZIF_RS_3DPC_ENTITAT .

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
  PRIVATE SECTION.

    DATA gs_params TYPE zif_rs_3dpc_abrufstatus=>ts_input_parameters .

    CLASS-METHODS get_abrufkopf_contract
      IMPORTING
        !is_entity_pos     TYPE zif_rs_3dpc_abrufstatus=>ts_input_parameters-er_entity_pos
      RETURNING
        VALUE(rv_contract) TYPE zif_rs_3dpc_abrufstatus=>ts_input_parameters-er_entity_kopf-einzelvertrag
      RAISING
        /iwbep/cx_mgw_tech_exception
        /iwbep/cx_mgw_busi_exception .
ENDCLASS.



CLASS ZCL_RS_3ABRUFSTATUS IMPLEMENTATION.


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
    CASE is_params-iv_entity_set_name.
      WHEN zif_rs_3constants=>gc_entityset_abrufkopfset.
        rv_object = zcl_rs_3abrufstatus_kopf=>create( is_params ).
      WHEN zif_rs_3constants=>gc_entityset_abrufposset.
        CASE get_abrufkopf_contract( is_params-er_entity_pos ).
          WHEN zif_rs_3constants=>gc_abruf_contract_lp2.
            rv_object = zcl_rs_3abrufstatus_lp2=>create( is_params ).
          WHEN zif_rs_3constants=>gc_abruf_contract_lp3.
            rv_object = zcl_rs_3abrufstatus_lp3=>create( is_params ).
          WHEN zif_rs_3constants=>gc_abruf_contract_lp5.
            rv_object = zcl_rs_3abrufstatus_lp5=>create( is_params ).
          WHEN OTHERS.
            rv_object = NEW zcl_rs_3abrufstatus( is_params ).
        ENDCASE.
      WHEN OTHERS.
        rv_object = NEW zcl_rs_3abrufstatus( is_params ).
    ENDCASE.
  ENDMETHOD.


  METHOD get_abrufkopf_contract.
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
    DATA(ls_entity_pos) = is_entity_pos.
    CLEAR rv_contract.
    IF ls_entity_pos IS INITIAL.
      RETURN.
    ENDIF.
    DATA ls_input_params TYPE zcl_rs_3abrufkopf=>ts_input_parameters.
    DATA(lo_abrufkopf) = NEW zcl_rs_3abrufkopf( ls_input_params ).
    DATA(lv_osql_where_clause) = lo_abrufkopf->mc_filter_values-abrufnummer_filter.
    REPLACE ALL OCCURRENCES: OF lo_abrufkopf->mc_filter_values-ampersand IN lv_osql_where_clause WITH ls_entity_pos-abrufnummer.
    CONDENSE lv_osql_where_clause.
    DATA(lt_entity) = lo_abrufkopf->get_db_values( lv_osql_where_clause ).
    IF lt_entity IS NOT INITIAL.
      rv_contract = lt_entity[ 1 ]-einzelvertrag.
    ENDIF.
  ENDMETHOD.


  method ZIF_RS_3DPC_ABRUFSTATUS~CHECK.
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
    es_entity_vorhaben = gs_params-er_entity_vorhaben.
    es_entity_kopf = gs_params-er_entity_kopf.
    es_entity_pos = gs_params-er_entity_pos.
    es_entity_billing = gs_params-er_entity_billing.
    es_entity_kopf_old =  gs_params-er_entity_kopf_old.
    es_entity_pos_old =  gs_params-er_entity_pos_old.
    es_email_params = gs_params-es_email_params.
    es_mo_context = gs_params-es_mo_context.
  endmethod.
ENDCLASS.
