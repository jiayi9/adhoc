
attr_list_inter_1 = c(
  "ANELVA_MACHINE",
  "BEARING_LOT_NUM",
  "CAPACITY",
  "CLAMP_SCREW_ID",
  "CLAMP_VEN_ID",
  "CMS_CONFIG",
  "DRIVE_LOCATION",
  "FIRMWARE_VER",
  "FSA_SUPPLIER_0",
  "FSA_SUPPLIER_1",
  "FSA_SUPPLIER_BY_HD",
  "HEAD_PHYS_PSN",
  "HGA_LOCATION_CODE",
  "HGA_SUPPLIER",
  "HSA_REV",
  "INTERFACE",
  "LINE_NUM",
  "MEDIA_CODE",
  "MEDIA_TYPE",
  "MEDIA_TYPE_BY_HD",
  "MOTOR_BASE",
  "MOTOR_HUB_VEN",
  "MOTOR_RAMP_VEN",
  "MOTOR_VEN_ID",
  "PRE_AMP_REV",
  "PRE_AMP_VENDOR",
  "PRIME",
  "SERVO_CODE",
  "SLDR_LOCATION_CODE",
  "SPUTTER_WC",
  "TOP_COVER_VEN",
  "WAFER_CODE",
  "WAFER_TYPE"
)

attr_list_inter_2 = gsub("_",".",attr_list_inter_1)

attr_list_inter = c(attr_list_inter_1,attr_list_inter_2)