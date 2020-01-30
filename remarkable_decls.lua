local ffi=require"ffi"
ffi.cdef[[
typedef enum _eink_ioctl_command {
  MXCFB_SET_WAVEFORM_MODES	           = 0x40484600 | 0x2B, // takes struct mxcfb_waveform_modes
  MXCFB_SET_TEMPERATURE		             = 0x40484600 | 0x2C, // takes int32_t
  MXCFB_SET_AUTO_UPDATE_MODE           = 0x40484600 | 0x2D, // takes __u32
  MXCFB_SEND_UPDATE                    = 0x40484600 | 0x2E, // takes struct mxcfb_update_data
  MXCFB_WAIT_FOR_UPDATE_COMPLETE       = 0x40484600 | 0x2F, // takes struct mxcfb_update_marker_data
  MXCFB_SET_PWRDOWN_DELAY              = 0x40484600 | 0x30, // takes int32_t
  MXCFB_GET_PWRDOWN_DELAY              = 0x40484600 | 0x31, // takes int32_t
  MXCFB_SET_UPDATE_SCHEME              = 0x40484600 | 0x32, // takes __u32
  MXCFB_GET_WORK_BUFFER                = 0x40484600 | 0x34, // takes unsigned long
  MXCFB_SET_TEMP_AUTO_UPDATE_PERIOD    = 0x40484600 | 0x36, // takes int32_t
  MXCFB_DISABLE_EPDC_ACCESS            = 0x40484600 | 0x35,
  MXCFB_ENABLE_EPDC_ACCESS             = 0x40484600 | 0x36
} eink_ioctl_command;
]]
ffi.cdef[[
typedef enum _auto_update_mode {
  AUTO_UPDATE_MODE_REGION_MODE         = 0,
  AUTO_UPDATE_MODE_AUTOMATIC_MODE      = 1
} auto_update_mode;

typedef enum _update_scheme {
  UPDATE_SCHEME_SNAPSHOT         = 0,
  UPDATE_SCHEME_QUEUE            = 1,
  UPDATE_SCHEME_QUEUE_AND_MERGE  = 2
} update_scheme;

typedef enum _update_mode
{
  UPDATE_MODE_PARTIAL   = 0,
  UPDATE_MODE_FULL      = 1
} update_mode;

typedef enum _waveform_mode {
  WAVEFORM_MODE_INIT         = 0x0,	                 /* Screen goes to white (clears) */
  WAVEFORM_MODE_GLR16			   = 0x4,                  /* Basically A2 (so partial refresh shouldnt be possible here) */
  WAVEFORM_MODE_GLD16			   = 0x5,                  /* Official -- and enables Regal D Processing */

  // Unsupported?
  WAVEFORM_MODE_DU           = 0x1,	                 /* [Direct Update] Grey->white/grey->black  -- remarkable uses this for drawing */
  WAVEFORM_MODE_GC16         = 0x2,	                 /* High fidelity (flashing) */
  WAVEFORM_MODE_GC4          = WAVEFORM_MODE_GC16,   /* For compatibility */
  WAVEFORM_MODE_GC16_FAST    = 0x3,                  /* Medium fidelity  -- remarkable uses this for UI */
  WAVEFORM_MODE_GL16_FAST    = 0x6,                  /* Medium fidelity from white transition */
  WAVEFORM_MODE_DU4          = 0x7,	                 /* Medium fidelity 4 level of gray direct update */
  WAVEFORM_MODE_REAGL	       = 0x8,	                 /* Ghost compensation waveform */
  WAVEFORM_MODE_REAGLD       = 0x9,	                 /* Ghost compensation waveform with dithering */
  WAVEFORM_MODE_GL4		       = 0xA,	                 /* 2-bit from white transition */
  WAVEFORM_MODE_GL16_INV		 = 0xB,	                 /* High fidelity for black transition */
  WAVEFORM_MODE_AUTO			   = 257                   /* Official */
} waveform_mode;

typedef enum _display_temp {
  TEMP_USE_AMBIENT           = 0x1000,
  TEMP_USE_PAPYRUS           = 0X1001,
  TEMP_USE_REMARKABLE_DRAW   = 0x0018,
  TEMP_USE_MAX               = 0xFFFF
} display_temp;

typedef struct {
  uint32_t top;
  uint32_t left;
  uint32_t width;
  uint32_t height;
} mxcfb_rect;

typedef struct {
	uint32_t update_marker;
	uint32_t collision_test;
} mxcfb_update_marker_data;

typedef struct {
	uint32_t phys_addr;
	uint32_t width;                   /* width of entire buffer */
	uint32_t height;	                /* height of entire buffer */
	mxcfb_rect alt_update_region;	    /* region within buffer to update */
} mxcfb_alt_buffer_data;

typedef struct {
	mxcfb_rect update_region;
  uint32_t waveform_mode;

  // Choose between FULL and PARTIAL
  uint32_t update_mode;

  // Checkpointing
  uint32_t update_marker;

  int temp;                         // 0x1001 = TEMP_USE_PAPYRUS
  unsigned int flags;               // 0x0000


  /*
   * Dither mode is entirely unused since the following means v1 is used not v2
   *
   * arch/arm/configs/zero-gravitas_defconfig
      173:CONFIG_FB_MXC_EINK_PANEL=y

     firmware/Makefile
      68:fw-shipped-$(CONFIG_FB_MXC_EINK_PANEL) += \

     drivers/video/fbdev/mxc/mxc_epdc_fb.c
      4969:#ifdef CONFIG_FB_MXC_EINK_AUTO_UPDATE_MODE
      5209:#ifdef CONFIG_FB_MXC_EINK_AUTO_UPDATE_MODE

     drivers/video/fbdev/mxc/mxc_epdc_v2_fb.c
      5428:#ifdef CONFIG_FB_MXC_EINK_AUTO_UPDATE_MODE
      5662:#ifdef CONFIG_FB_MXC_EINK_AUTO_UPDATE_MODE

     drivers/video/fbdev/mxc/Makefile
      10:obj-$(CONFIG_FB_MXC_EINK_PANEL)      += mxc_epdc_fb.o
      11:obj-$(CONFIG_FB_MXC_EINK_V2_PANEL)   += mxc_epdc_v2_fb.o
   *
   */
  int dither_mode;
	int quant_bit; // used only when dither_mode is > PASSTHROUGH and < MAX

  mxcfb_alt_buffer_data alt_buffer_data;  // not used when flags is 0x0000
} mxcfb_update_data;

typedef enum _mxcfb_dithering_mode {
	EPDC_FLAG_USE_DITHERING_PASSTHROUGH = 0x0,
	EPDC_FLAG_USE_DITHERING_DRAWING     = 0x1,
	// Dithering Processing (Version 1.0 - for i.MX508 and i.MX6SL)
  EPDC_FLAG_USE_DITHERING_Y1          = 0x002000,
  EPDC_FLAG_USE_REMARKABLE_DITHER     = 0x300f30,
  EPDC_FLAG_USE_DITHERING_Y4          = 0x004000

} mxcfb_dithering_mode;
]]
