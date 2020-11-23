INTERFACE zif_rs_3dpc_antwort
*----------------------------------------------------------------------*
* SBB AG, SAP CCC-IT, Bern/Berne/Berna
*----------------------------------------------------------------------*
*
*-----------Änderungsdokumentation / documentation of changes----------*
* Version SAP-Rel Datum Bearbeiter
* Beschreibung Get user list for project
*----------------------------------------------------------------------*
* >000< 7.50 tt.mm.jjjj Firma(DXC Technology) / Name(Sigried Canezal)
* Programmerstellung / creation of program
*----------------------------------------------------------------------*
* >001< 7.50 tt.mm.jjjj Firma(company) / Name(name)
* ...
*----------------------------------------------------------------------*
  PUBLIC .


  METHODS get_response_post
    EXPORTING
      !es_entity     TYPE any
      !es_mo_context TYPE REF TO /iwbep/if_mgw_context
    RAISING
      /iwbep/cx_mgw_tech_exception
      /iwbep/cx_mgw_busi_exception .
  METHODS get_response_update
    EXPORTING
      !es_entity     TYPE any
      !es_mo_context TYPE REF TO /iwbep/if_mgw_context
    RAISING
      /iwbep/cx_mgw_tech_exception
      /iwbep/cx_mgw_busi_exception .
  METHODS get_response_delete
    EXPORTING
      !es_mo_context TYPE REF TO /iwbep/if_mgw_context
    RAISING
      /iwbep/cx_mgw_tech_exception
      /iwbep/cx_mgw_busi_exception .
  METHODS get_response_read
    EXPORTING
      !es_entity           TYPE any
      !es_response_context TYPE /iwbep/if_mgw_appl_srv_runtime=>ty_s_mgw_response_entity_cntxt
      !es_mo_context       TYPE REF TO /iwbep/if_mgw_context
    RAISING
      /iwbep/cx_mgw_tech_exception
      /iwbep/cx_mgw_busi_exception .
  METHODS get_response_query
    EXPORTING
      !es_t_entity         TYPE STANDARD TABLE
      !es_response_context TYPE /iwbep/if_mgw_appl_srv_runtime=>ty_s_mgw_response_context
      !es_mo_context       TYPE REF TO /iwbep/if_mgw_context
    RAISING
      /iwbep/cx_mgw_tech_exception
      /iwbep/cx_mgw_busi_exception .
  METHODS get_response_read_expand
    EXPORTING
      !es_entity                  TYPE REF TO data
      !es_response_context        TYPE /iwbep/if_mgw_appl_srv_runtime=>ty_s_mgw_response_entity_cntxt
      !es_t_expanded_clauses      TYPE string_table
      !es_t_expanded_tech_clauses TYPE string_table
      !es_mo_context              TYPE REF TO /iwbep/if_mgw_context
    RAISING
      /iwbep/cx_mgw_tech_exception
      /iwbep/cx_mgw_busi_exception .
  METHODS get_response_query_expand
    EXPORTING
      !es_entityset               TYPE REF TO data
      !es_t_expanded_clauses      TYPE string_table
      !es_t_expanded_tech_clauses TYPE string_table
      !es_response_context        TYPE /iwbep/if_mgw_appl_srv_runtime=>ty_s_mgw_response_context
      !es_mo_context              TYPE REF TO /iwbep/if_mgw_context
    RAISING
      /iwbep/cx_mgw_tech_exception
      /iwbep/cx_mgw_busi_exception .
  METHODS get_response_post_deep_entity
    EXPORTING
      !es_r_deep_entity TYPE REF TO data
      !es_mo_context    TYPE REF TO /iwbep/if_mgw_context
    RAISING
      /iwbep/cx_mgw_tech_exception
      /iwbep/cx_mgw_busi_exception .
ENDINTERFACE.
