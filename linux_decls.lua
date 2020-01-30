local ffi=require"ffi"
ffi.cdef[[
struct fb_fix_screeninfo {
	char id[16];			/* identification string eg "TT Builtin" */
	unsigned long smem_start;	/* Start of frame buffer mem */
					/* (physical address) */
	uint32_t smem_len;			/* Length of frame buffer mem */
	uint32_t type;			/* see FB_TYPE_*		*/
	uint32_t type_aux;			/* Interleave for interleaved Planes */
	uint32_t visual;			/* see FB_VISUAL_*		*/ 
	uint16_t xpanstep;			/* zero if no hardware panning  */
	uint16_t ypanstep;			/* zero if no hardware panning  */
	uint16_t ywrapstep;		/* zero if no hardware ywrap    */
	uint32_t line_length;		/* length of a line in bytes    */
	unsigned long mmio_start;	/* Start of Memory Mapped I/O   */
					/* (physical address) */
	uint32_t mmio_len;			/* Length of Memory Mapped I/O  */
	uint32_t accel;			/* Indicate to driver which	*/
					/*  specific chip/card we have	*/
	uint16_t capabilities;		/* see FB_CAP_*			*/
	uint16_t reserved[2];		/* Reserved for future compatibility */
};
struct fb_bitfield {
	uint32_t offset;			/* beginning of bitfield	*/
	uint32_t length;			/* length of bitfield		*/
	uint32_t msb_right;		/* != 0 : Most significant bit is */ 
					/* right */ 
};
struct fb_var_screeninfo {
	uint32_t xres;			/* visible resolution		*/
	uint32_t yres;
	uint32_t xres_virtual;		/* virtual resolution		*/
	uint32_t yres_virtual;
	uint32_t xoffset;			/* offset from virtual to visible */
	uint32_t yoffset;			/* resolution			*/

	uint32_t bits_per_pixel;		/* guess what			*/
	uint32_t grayscale;		/* 0 = color, 1 = grayscale,	*/
					/* >1 = FOURCC			*/
	struct fb_bitfield red;		/* bitfield in fb mem if true color, */
	struct fb_bitfield green;	/* else only length is significant */
	struct fb_bitfield blue;
	struct fb_bitfield transp;	/* transparency			*/	

	uint32_t nonstd;			/* != 0 Non standard pixel format */

	uint32_t activate;			/* see FB_ACTIVATE_*		*/

	uint32_t height;			/* height of picture in mm    */
	uint32_t width;			/* width of picture in mm     */

	uint32_t accel_flags;		/* (OBSOLETE) see fb_info.flags */

	/* Timing: All values in pixclocks, except pixclock (of course) */
	uint32_t pixclock;			/* pixel clock in ps (pico seconds) */
	uint32_t left_margin;		/* time from sync to picture	*/
	uint32_t right_margin;		/* time from picture to sync	*/
	uint32_t upper_margin;		/* time from sync to picture	*/
	uint32_t lower_margin;
	uint32_t hsync_len;		/* length of horizontal sync	*/
	uint32_t vsync_len;		/* length of vertical sync	*/
	uint32_t sync;			/* see FB_SYNC_*		*/
	uint32_t vmode;			/* see FB_VMODE_*		*/
	uint32_t rotate;			/* angle we rotate counter clockwise */
	uint32_t colorspace;		/* colorspace for FOURCC-based modes */
	uint32_t reserved[4];		/* Reserved for future compatibility */
};
]]
return { FBIO = ffi.new[[struct {
static const int GET_VSCREENINFO = 0x4600;
static const int PUT_VSCREENINFO = 0x4601;
static const int GET_FSCREENINFO = 0x4602;
}]],
EV = ffi.new[[struct {
static const int SYN = 0x00;
static const int KEY = 0x01;
static const int REL = 0x02;
static const int ABS = 0x03;
static const int MSC = 0x04;
static const int SW = 0x05;
static const int LED = 0x11;
static const int SND = 0x12;
static const int REP = 0x14;
static const int FF = 0x15;
static const int PWR = 0x16;
static const int FF_STATUS = 0x17;
static const int MAX = 0x1f;
static const int VERSION = 0x010001;
}]],
ABS = ffi.new[[struct {
static const int MT_SLOT = 0x2f;
static const int MT_TOUCH_MAJOR = 0x30;
static const int MT_TOUCH_MINOR = 0x31;
static const int MT_WIDTH_MAJOR = 0x32;
static const int MT_WIDTH_MINOR = 0x33;
static const int MT_ORIENTATION = 0x34;
static const int MT_POSITION_X = 0x35;
static const int MT_POSITION_Y = 0x36;
static const int MT_TOOL_TYPE = 0x37;
static const int MT_BLOB_ID = 0x38;
static const int MT_TRACKING_ID = 0x39;
static const int MT_PRESSURE = 0x3a;
static const int MT_DISTANCE = 0x3b;
static const int MT_TOOL_X = 0x3c;
static const int MT_TOOL_Y = 0x3d;
}]],
FB_TYPE = ffi.new[[struct {
static const int PACKED_PIXELS = 0;
static const int PLANES = 1;
static const int INTERLEAVED_PLANES = 2;
static const int TEXT = 3;
static const int VGA_PLANES = 4;
static const int FOURCC = 5;
}]],
FB_VISUAL = ffi.new[[struct {
static const int MONO01 = 0;
static const int MONO10 = 1;
static const int TRUECOLOR = 2;
static const int PSEUDOCOLOR = 3;
static const int DIRECTCOLOR = 4;
static const int STATIC_PSEUDOCOLOR = 5;
static const int FOURCC = 6;
}]],
}
