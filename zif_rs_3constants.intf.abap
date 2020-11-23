INTERFACE zif_rs_3constants
  PUBLIC .


  CONSTANTS gc_role_authobject TYPE xuobject VALUE 'ZRS_3TTROL' ##NO_TEXT.
  CONSTANTS gc_role_authfield TYPE xufield VALUE 'ZRS_TTOOL' ##NO_TEXT.
  CONSTANTS gc_verrechnung_appl TYPE zse_2_application VALUE 'TRICHTER' ##NO_TEXT.
  CONSTANTS gc_verrechnung_name TYPE zse_2_name VALUE 'RFC_ECC' ##NO_TEXT.
  CONSTANTS gc_verrechnung_dep TYPE flag VALUE 'X' ##NO_TEXT.
  CONSTANTS gc_message_class TYPE rsdag-arbgb VALUE 'ZRS_3' ##NO_TEXT.
  CONSTANTS gc_email_project_dat TYPE zse_mail_id VALUE 'ZRS_TRICHTER_PROJECT_START_DATE' ##NO_TEXT.
  CONSTANTS gc_email_project_forecast TYPE zse_mail_id VALUE 'ZRS_TRICHTER_PROJECT_FORECAST' ##NO_TEXT.
  CONSTANTS gc_artikel_nr_range TYPE inri-nrrangenr VALUE '01' ##NO_TEXT.
  CONSTANTS gc_artikel_nr_object TYPE inri-object VALUE 'ZRS_3TTART' ##NO_TEXT.
  CONSTANTS gc_vorhaben_nr_object TYPE inri-object VALUE 'ZRS_3TTPRJ' ##NO_TEXT.
  CONSTANTS gc_vorhaben_nr_range TYPE inri-nrrangenr VALUE '01' ##NO_TEXT.
  CONSTANTS gc_role_devspoc TYPE zrs_3tt_role VALUE 'DSPOC' ##NO_TEXT.
  CONSTANTS gc_role_dxcrun TYPE zrs_3tt_role VALUE 'DXRUN' ##NO_TEXT.
  CONSTANTS gc_role_plswa TYPE zrs_3tt_role VALUE 'PLSWA' ##NO_TEXT.
  CONSTANTS gc_role_smsbb TYPE zrs_3tt_role VALUE 'SMSBB' ##NO_TEXT.
  CONSTANTS gc_entityset_mdartikelset TYPE string VALUE 'MDArtikelSet' ##NO_TEXT.
  CONSTANTS gc_message_type_error TYPE bapiret2-type VALUE 'E' ##NO_TEXT.
  CONSTANTS gc_message_type_success TYPE bapiret2-type VALUE 'S' ##NO_TEXT.
  CONSTANTS gc_message_type_warning TYPE bapiret2-type VALUE 'W' ##NO_TEXT.
  CONSTANTS gc_entityset_benutzerrollenset TYPE string VALUE 'BenutzerrollenSet' ##NO_TEXT.
  CONSTANTS gc_abruf_nr_range TYPE inri-nrrangenr VALUE '01' ##NO_TEXT.
  CONSTANTS gc_abruf_nr_object TYPE inri-object VALUE 'ZRS_3TTABR' ##NO_TEXT.
  CONSTANTS gc_abruf_new_status TYPE zrs_3tt_de_statusabrf VALUE '10' ##NO_TEXT.
  CONSTANTS gc_entityset_abrufkopfset TYPE string VALUE 'AbrufKopfSet' ##NO_TEXT.
  CONSTANTS gc_entityset_abrufposset TYPE string VALUE 'AbrufPosSet' ##NO_TEXT.
  CONSTANTS gc_abruf_contract_lp2 TYPE zrs_3tt_de_einzlvrtrg VALUE 'LP2' ##NO_TEXT.
  CONSTANTS gc_abruf_contract_lp3 TYPE zrs_3tt_de_einzlvrtrg VALUE 'LP3' ##NO_TEXT.
  CONSTANTS gc_abruf_contract_lp5 TYPE zrs_3tt_de_einzlvrtrg VALUE 'LP5' ##NO_TEXT.
  CONSTANTS gc_vorhaben_status_active TYPE zrs_3tt_de_status VALUE 'V0003' ##NO_TEXT.
  CONSTANTS gc_einheit_forecast_aw TYPE zrs_3tt_de_einhtfraw VALUE 'AW' ##NO_TEXT.
  CONSTANTS gc_einheit_forecast_tp TYPE zrs_3tt_de_einhtfrtp VALUE 'TP' ##NO_TEXT.
  CONSTANTS gc_procezzart_type_abruf TYPE char01 VALUE 'A' ##NO_TEXT.
  CONSTANTS:
    BEGIN OF gc_procezzart_type_abrufpos,
      lp2 TYPE char01 VALUE '2',
      lp3 TYPE char01 VALUE '3',
      lp5 TYPE char01 VALUE '5',
    END OF gc_procezzart_type_abrufpos .
  CONSTANTS gc_range_sign_include TYPE char01 VALUE 'I' ##NO_TEXT.
  CONSTANTS gc_range_option_equal TYPE string VALUE 'EQ' ##NO_TEXT.
  CONSTANTS gc_range_option_between TYPE string VALUE 'BT' ##NO_TEXT.
  CONSTANTS:
    BEGIN OF gc_cud_types,
      create TYPE string VALUE 'C' ##NO_TEXT,
      update TYPE string VALUE 'U' ##NO_TEXT,
      delete TYPE string VALUE 'D' ##NO_TEXT,
    END OF gc_cud_types ,
    BEGIN OF gc_einheit_antiel,
      percentage TYPE zrs_3tt_de_einhtantl VALUE '%',
    END OF gc_einheit_antiel.
  CONSTANTS:
    BEGIN OF gc_email_types,
      u TYPE string VALUE 'U' ##NO_TEXT,
    END OF gc_email_types .
  CONSTANTS gc_entityset_vorhabenforecast TYPE string VALUE 'VorhabenforecastSet' ##NO_TEXT.
  CONSTANTS gc_update_entity TYPE /iwbep/mgw_operation_type VALUE 'UE' ##NO_TEXT.
  CONSTANTS gc_patch_entity TYPE /iwbep/mgw_operation_type VALUE 'PE' ##NO_TEXT.
  CONSTANTS:
    BEGIN OF gc_session_expires_secs,
      header     TYPE n LENGTH 5 VALUE 300 ##NO_TEXT,
      position   TYPE n LENGTH 5 VALUE 500 ##NO_TEXT,
      multiplier TYPE n LENGTH 2 VALUE 12,
    END OF gc_session_expires_secs .
  CONSTANTS:
    BEGIN OF gc_abrufkopf_status,
      s10  TYPE zrs_3tt_de_status VALUE '10' ##NO_TEXT,
      s15  TYPE zrs_3tt_de_status VALUE '15' ##NO_TEXT,
      s900 TYPE zrs_3tt_de_status VALUE '900' ##NO_TEXT,
      s915 TYPE zrs_3tt_de_status VALUE '915' ##NO_TEXT,
    END OF gc_abrufkopf_status .

  CONSTANTS:
    BEGIN OF gc_katlieft_status,
      go_live   TYPE string VALUE 'GO_LIVE' ##NO_TEXT,
      impl      TYPE string VALUE 'IMPL' ##NO_TEXT,
      spez      TYPE string VALUE 'SPEZ' ##NO_TEXT,
      spez_impl TYPE string VALUE 'SPEZ_IMPL' ##NO_TEXT,
      story     TYPE string VALUE 'STORY' ##NO_TEXT,
      tstpl     TYPE string VALUE 'TSTPL' ##NO_TEXT,
    END OF gc_katlieft_status .
  CONSTANTS:
    BEGIN OF gc_abrufpos_status_lp2,
      s10  TYPE zrs_3tt_de_status VALUE '10' ##NO_TEXT,
      s20  TYPE zrs_3tt_de_status VALUE '20' ##NO_TEXT,
      s21  TYPE zrs_3tt_de_status VALUE '21' ##NO_TEXT,
      s22  TYPE zrs_3tt_de_status VALUE '22' ##NO_TEXT,
      s30  TYPE zrs_3tt_de_status VALUE '30' ##NO_TEXT,
      s31  TYPE zrs_3tt_de_status VALUE '31' ##NO_TEXT,
      s32  TYPE zrs_3tt_de_status VALUE '32' ##NO_TEXT,
      s35  TYPE zrs_3tt_de_status VALUE '35' ##NO_TEXT,
      s36  TYPE zrs_3tt_de_status VALUE '36' ##NO_TEXT,
      s37  TYPE zrs_3tt_de_status VALUE '37' ##NO_TEXT,
      s40  TYPE zrs_3tt_de_status VALUE '40' ##NO_TEXT,
      s41  TYPE zrs_3tt_de_status VALUE '41' ##NO_TEXT,
      s42  TYPE zrs_3tt_de_status VALUE '42' ##NO_TEXT,
      s900 TYPE zrs_3tt_de_status VALUE '900' ##NO_TEXT,
      s910 TYPE zrs_3tt_de_status VALUE '910' ##NO_TEXT,
    END OF gc_abrufpos_status_lp2 .
  CONSTANTS:
    BEGIN OF gc_abrufpos_status_lp3,
      s10  TYPE zrs_3tt_de_status VALUE '10' ##NO_TEXT,
      s110 TYPE zrs_3tt_de_status VALUE '110' ##NO_TEXT,
      s111 TYPE zrs_3tt_de_status VALUE '111' ##NO_TEXT,
      s112 TYPE zrs_3tt_de_status VALUE '112' ##NO_TEXT,
      s113 TYPE zrs_3tt_de_status VALUE '113' ##NO_TEXT,
      s114 TYPE zrs_3tt_de_status VALUE '114' ##NO_TEXT,
      s900 TYPE zrs_3tt_de_status VALUE '900' ##NO_TEXT,
      s910 TYPE zrs_3tt_de_status VALUE '910' ##NO_TEXT,
    END OF gc_abrufpos_status_lp3 .
  CONSTANTS:
    BEGIN OF gc_vorhaben_status,
      sv0001 TYPE j_estat VALUE 'V0001' ##NO_TEXT,
      sv0002 TYPE j_estat VALUE 'V0002' ##NO_TEXT,
      sv0003 TYPE j_estat VALUE 'V0003' ##NO_TEXT,
      sv0004 TYPE j_estat VALUE 'V0004' ##NO_TEXT,
    END OF gc_vorhaben_status .
  CONSTANTS: BEGIN OF gc_email_templates_abruf,
               et21020   TYPE zse_mail_id VALUE 'ZRS_TRICHTER_21020' ##NO_TEXT,
               et22021   TYPE zse_mail_id VALUE 'ZRS_TRICHTER_22021' ##NO_TEXT,
               et22122   TYPE zse_mail_id VALUE 'ZRS_TRICHTER_22122' ##NO_TEXT,
               et22120   TYPE zse_mail_id VALUE 'ZRS_TRICHTER_22120' ##NO_TEXT,
               et220900  TYPE zse_mail_id VALUE 'ZRS_TRICHTER_220900' ##NO_TEXT,
               et310110  TYPE zse_mail_id VALUE 'ZRS_TRICHTER_30001' ##NO_TEXT,
               et311010  TYPE zse_mail_id VALUE 'ZRS_TRICHTER_30003' ##NO_TEXT,
               et3110111 TYPE zse_mail_id VALUE 'ZRS_TRICHTER_30004' ##NO_TEXT,
               et3111112 TYPE zse_mail_id VALUE 'ZRS_TRICHTER_30005' ##NO_TEXT,
               et3112113 TYPE zse_mail_id VALUE 'ZRS_TRICHTER_30007' ##NO_TEXT,
               et3113114 TYPE zse_mail_id VALUE 'ZRS_TRICHTER_30008' ##NO_TEXT,
*TYPE ZSE_MAIL_ID VALUE 'ZRS_TRICHTER_30009'##NO_TEXT,
             END OF gc_email_templates_abruf,
             BEGIN OF gc_email_templates_vorhaben,
               et_forecast   TYPE zse_mail_id VALUE 'ZRS_TRICHTER_PROJECT_FORECAST' ##NO_TEXT,
               et_start_date TYPE zse_mail_id VALUE 'ZRS_TRICHTER_PROJECT_START_DATE' ##NO_TEXT,
             END OF gc_email_templates_vorhaben,
             BEGIN OF gc_email_kind,
               address_type_u    TYPE zse_adr_type VALUE 'U' ##NO_TEXT,
               recepient_type_to TYPE zse_rec_type VALUE 'A' ##NO_TEXT,
               recepient_type_cc TYPE zse_rec_type VALUE 'C' ##NO_TEXT,
             END OF gc_email_kind,
             BEGIN OF gc_email_sender,
               no_reply TYPE bapiaddr3-e_mail VALUE 'noreply@sbb.ch' ##NO_TEXT,
             END OF gc_email_sender,
             BEGIN OF gc_email_namevalues_abruf,
               vorhabensnummer    TYPE string VALUE 'VORN' ##NO_TEXT,
               projektbezeichnung TYPE string VALUE 'VOBE' ##NO_TEXT,
               abrufnummer        TYPE string VALUE 'ABRN' ##NO_TEXT,
               notiz              TYPE string VALUE 'NOTE' ##NO_TEXT,
               projekt_start      TYPE string VALUE 'PSTA' ##NO_TEXT,
               entwicklungsstart  TYPE string VALUE 'PDEV' ##NO_TEXT,
             END OF gc_email_namevalues_abruf.

ENDINTERFACE.
