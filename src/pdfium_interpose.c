#define _GNU_SOURCE

#include "pdfium/fpdfview.h"
#include "interpose/include/interpose.h"

// NOTE: the .so file built from this source file is supposed to be preloaded to 'xochitl'
//  which runs as root on the reMarkable tablet. Be as simple and defensive as possible.

#include <errno.h>
#include <math.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

#include <sys/stat.h>
#include <sys/types.h>
#include <unistd.h>

#define SCREEN_WIDTH 1404
#define SCREEN_HEIGHT 1872

#define UID_GID 1000  // User and group ID of first custom user

#define PrintAndExit(Code, Fmt, ...) \
    do { \
        fprintf(stderr, "ljrM_interpose.so: " Fmt "\n", __VA_ARGS__); \
        _Exit(Code); \
    } while (0)

#define Printf printf

static __attribute__((constructor))
void Init() {
    static const mode_t Mode =
        S_IRWXU | S_IRWXG |  // owner + group: rwx
        S_ISVTX;             // sticky (restricted deletion)

    static const char *Dir = "/tmp/ljrM-pdf";

    // Allow creation of group-writable directory:
    const mode_t oldUmask = umask(S_IWOTH);

    if (mkdir(Dir, Mode) == -1) {
        if (errno != EEXIST)
            PrintAndExit(200, "failed to create directory '%s': %s", Dir, strerror(errno));

        struct stat st;

        if (stat(Dir, &st) == -1)
            PrintAndExit(201, "failed to stat '%s': %s", Dir, strerror(errno));

        if ((st.st_mode & S_IFMT) != S_IFDIR)
            PrintAndExit(202, "'%s' is not a directory", Dir);
        else if (st.st_uid != 0)
            PrintAndExit(203, "'%s' is not owned by root", Dir);
        else if (st.st_gid != UID_GID)
            PrintAndExit(204, "'%s' is not group-owned by group with ID %d", Dir, UID_GID);
        else if ((st.st_mode & 07777) != Mode)
            PrintAndExit(205, "'%s' has mode flags 0%o, expected 0%o", Dir, (st.st_mode & 07777), Mode);
    }

    umask(oldUmask);

    if (chown(Dir, 0, UID_GID) == -1)
        PrintAndExit(206, "failed changing owner of '%s': %s", Dir, strerror(errno));
}

static
double GetScale(double width, double height) {
    return (height > 0.0 && width > 0.0) ?
        fmin(SCREEN_HEIGHT/height, SCREEN_WIDTH/width) :
        0.0;
}

static
double GetScaleForPage(FPDF_PAGE page) {
    return GetScale(FPDF_GetPageWidth(page), FPDF_GetPageHeight(page));
}

static
double Equals(double val, double refVal, double maxRelativeDeviation) {
    return (isfinite(refVal) && refVal != 0.0 &&
            fabs((val / refVal) - 1.0) <= maxRelativeDeviation);
}

////////// Tracking (Document -> file name, page -> document) //////////

#define PATH_PREFIX_LEN 42
/* Expecting UUID:
xxxxxxxx-xxxx-xxxx-xxxx-xxxxxxxxxxxx.pdf
*/
#define BASE_NAME_LEN 40
#define FILE_NAME_LEN (PATH_PREFIX_LEN + 1 + BASE_NAME_LEN)

static const char ExpectedPathPrefix[PATH_PREFIX_LEN + 1] = "/home/root/.local/share/remarkable/xochitl\0";
typedef char XoName[BASE_NAME_LEN + 1];

static bool IsExpectedFileName(const char *fileName) {
    return fileName && strlen(fileName) == FILE_NAME_LEN &&
        memcmp(fileName, ExpectedPathPrefix, PATH_PREFIX_LEN) == 0 &&
        fileName[PATH_PREFIX_LEN] == '/' &&
        strchr(fileName + PATH_PREFIX_LEN + 1, '/') == NULL &&
        // TODO: check that the base name is in UUID form.
        strcmp(fileName + FILE_NAME_LEN - 4, ".pdf") == 0;
}

static const char *GetName(const char *fileName) {
    return IsExpectedFileName(fileName) ?
        fileName + PATH_PREFIX_LEN + 1 :
        NULL;
}

typedef struct {
    FPDF_PAGE page;
    FPDF_DOCUMENT doc;
} PageAndDoc;

typedef struct {
    FPDF_DOCUMENT doc;
    XoName baseName;
} DocAndName;

#define MAX_TRACKED 16

#define SEARCH_MAP(Map, KeyMemberName, searchKey) \
    for (int i = 0; i < MAX_TRACKED; i++) \
        if (Map[i].KeyMemberName == searchKey)

static PageAndDoc DocumentMap[MAX_TRACKED];
static DocAndName NameMap[MAX_TRACKED];

static void SetDocumentMapEntry(FPDF_PAGE searchPage, FPDF_PAGE page, FPDF_DOCUMENT doc) {
    SEARCH_MAP(DocumentMap, page, searchPage) {
        DocumentMap[i].page = page;
        DocumentMap[i].doc = doc;
        return;
    }
}

static void SetNameMapEntry(FPDF_DOCUMENT searchDoc, FPDF_DOCUMENT doc, const char *name) {
    SEARCH_MAP(NameMap, doc, searchDoc) {
        NameMap[i].doc = doc;
        char *destPtr = NameMap[i].baseName;

        if (!name)
            memset(destPtr, 0, BASE_NAME_LEN + 1);
        else if (strlen(name) == BASE_NAME_LEN)
            memcpy(destPtr, name, BASE_NAME_LEN + 1);

        return;
    }
}

// ----------

static void ClearDocForPage(FPDF_PAGE page) {
    if (page)
        SetDocumentMapEntry(page, NULL, NULL);
}

static void ClearNameForDoc(FPDF_DOCUMENT doc) {
    if (doc)
        SetNameMapEntry(doc, NULL, NULL);
}

static void SetDocForPage(FPDF_PAGE page, FPDF_DOCUMENT doc) {
    if (page && doc)
        SetDocumentMapEntry(NULL, page, doc);
}

static void SetNameForDoc(FPDF_DOCUMENT doc, const char *name) {
    if (doc && name)
        SetNameMapEntry(NULL, doc, name);
}

static FPDF_DOCUMENT GetDocForPage(FPDF_PAGE page) {
    if (page)
        SEARCH_MAP(DocumentMap, page, page)
            return DocumentMap[i].doc;

    return NULL;
}

static const char *GetNameForDoc(FPDF_DOCUMENT doc) {
    if (doc)
        SEARCH_MAP(NameMap, doc, doc)
            return NameMap[i].baseName;

    return NULL;
}

//////////

INTERPOSE_C(FPDF_DOCUMENT, FPDF_LoadDocument,
            (FPDF_STRING file_path, FPDF_BYTESTRING password),
            (file_path, password)) {
    FPDF_DOCUMENT doc = Real__FPDF_LoadDocument(file_path, password);
    const char *name = GetName(file_path);

    Printf("INFO: LoadDocument: %s -> doc=%p\n"
           "                    expected=%s\n",
           file_path ? file_path : "(null)", (void *)doc,
           name ? "yes" : "no");

    SetNameForDoc(doc, name);

    return doc;
}

INTERPOSE_C_VOID(FPDF_CloseDocument, (FPDF_DOCUMENT document), (document)) {
    Printf("INFO: CloseDocument: doc=%p\n", (void *)document);

    Real__FPDF_CloseDocument(document);

    ClearNameForDoc(document);
}

INTERPOSE_C(FPDF_PAGE, FPDF_LoadPage, (FPDF_DOCUMENT document, int page_index),
            (document, page_index)) {
    FPDF_PAGE page = Real__FPDF_LoadPage(document, page_index);
    Printf("INFO: LoadPage: doc=%p, i=%d -> page=%p\n", (void *)document, page_index, (void *)page);

    const double width = FPDF_GetPageWidth(page);
    const double height = FPDF_GetPageHeight(page);

    Printf("      dimensions => %f x %f\n"
           "      scale => %f\n",
           width, height, GetScale(width, height));

    if (GetNameForDoc(document))
        SetDocForPage(page, document);

    return page;
}

INTERPOSE_C_VOID(FPDF_ClosePage, (FPDF_PAGE page), (page)) {
    Printf("INFO: ClosePage: page=%p\n", (void *)page);

    ClearDocForPage(page);

    Real__FPDF_ClosePage(page);
}

INTERPOSE_C(FPDF_BITMAP, FPDFBitmap_CreateEx,
            (int width, int height, int format, void* first_scan, int stride),
            (width, height, format, first_scan, stride)) {
    FPDF_BITMAP bitmap = Real__FPDFBitmap_CreateEx(width, height, format, first_scan, stride);
    Printf("INFO: CreateEx: w=%d h=%d format=%d ptr=%p stride=%d -> bitmap=%p\n",
           width, height, format, first_scan, stride, (void *)bitmap);
    return bitmap;
}

typedef double Millisecs;

Millisecs curTimeMs() {
    struct timespec t;
    clock_gettime(CLOCK_MONOTONIC, &t);
    return (1000000000ull * t.tv_sec + t.tv_nsec) / 1e6;
}

INTERPOSE_C_VOID(FPDF_RenderPageBitmapWithMatrix,
                 (FPDF_BITMAP bitmap, FPDF_PAGE page, const FS_MATRIX* matrix, const FS_RECTF* clipping, int flags),
                 (bitmap, page, matrix, clipping, flags)) {
    static const FS_MATRIX zeroMatrix;
    static const FS_RECTF zeroClip;

    const FS_MATRIX *m = matrix ? matrix : &zeroMatrix;
    const FS_RECTF *c = clipping ? clipping : &zeroClip;

    const double scale = GetScaleForPage(page);
    const bool isNoZoom = (Equals(m->a, scale, 0.001) && Equals(m->d, scale, 0.001) &&
                           m->b == 0.0 && m->c == 0.0 &&
                           m->e == 0.0 && m->f == 0.0);

    const FPDF_DOCUMENT doc = isNoZoom ? GetDocForPage(page) : NULL;
    const char *key = GetNameForDoc(doc);

    Printf("INFO: Render: m = [%f %f 0;  bitmap=%p, flags=0x%x\n"
           "                   %f %f 0;%s\n"
           "                   %f %f 1]\n"
           "              clip = (%f, %f)--(%f, %f)\n"
           "              key = %s\n",
           m->a, m->b, (void *)bitmap, flags, m->c, m->d,
           isNoZoom ? "  [NO ZOOM]" : "", m->e, m->f,
           c->left, c->top, c->right, c->bottom,
           key ? key : "(null)");

    const Millisecs startMs = curTimeMs();
    Real__FPDF_RenderPageBitmapWithMatrix(bitmap, page, matrix, clipping, flags);
    Printf("INFO: Rendered in %.02f ms\n", curTimeMs()-startMs);
}
