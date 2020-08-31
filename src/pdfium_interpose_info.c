#define _GNU_SOURCE

#include "pdfium/fpdfview.h"
#include "interpose/include/interpose.h"

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
        fprintf(stderr, "ljrM_interpose_info.so: " Fmt "\n", __VA_ARGS__); \
        _Exit(Code); \
    } while (0)

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

INTERPOSE_C(FPDF_DOCUMENT, FPDF_LoadDocument,
            (FPDF_STRING file_path, FPDF_BYTESTRING password),
            (file_path, password)) {
    FPDF_DOCUMENT doc = Real__FPDF_LoadDocument(file_path, password);
    printf("INFO: LoadDocument: %s -> doc=%p\n", file_path ? file_path : "(null)", (void *)doc);
    return doc;
}

INTERPOSE_C_VOID(FPDF_CloseDocument, (FPDF_DOCUMENT document), (document)) {
    printf("INFO: CloseDocument: doc=%p\n", (void *)document);
    Real__FPDF_CloseDocument(document);
}

INTERPOSE_C(FPDF_PAGE, FPDF_LoadPage, (FPDF_DOCUMENT document, int page_index),
            (document, page_index)) {
    FPDF_PAGE page = Real__FPDF_LoadPage(document, page_index);
    printf("INFO: LoadPage: doc=%p, i=%d -> page=%p\n", (void *)document, page_index, (void *)page);
    const double width = FPDF_GetPageWidth(page);
    const double height = FPDF_GetPageHeight(page);
    printf("      dimensions => %f x %f\n"
           "      scale => %f\n",
           width, height, GetScale(width, height));
    return page;
}

INTERPOSE_C_VOID(FPDF_ClosePage, (FPDF_PAGE page), (page)) {
    printf("INFO: ClosePage: page=%p\n", (void *)page);
    Real__FPDF_ClosePage(page);
}

INTERPOSE_C(FPDF_BITMAP, FPDFBitmap_CreateEx,
            (int width, int height, int format, void* first_scan, int stride),
            (width, height, format, first_scan, stride)) {
    FPDF_BITMAP bitmap = Real__FPDFBitmap_CreateEx(width, height, format, first_scan, stride);
    printf("INFO: CreateEx: w=%d h=%d format=%d ptr=%p stride=%d -> bitmap=%p\n",
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

    printf("INFO: Render: m = [%f %f 0;  bitmap=%p, flags=0x%x\n"
           "                   %f %f 0;%s\n"
           "                   %f %f 1]\n"
           "              clip = (%f, %f)--(%f, %f)\n",
           m->a, m->b, (void *)bitmap, flags, m->c, m->d,
           isNoZoom ? "  [NO ZOOM]" : "", m->e, m->f,
           c->left, c->top, c->right, c->bottom);

    const Millisecs startMs = curTimeMs();
    Real__FPDF_RenderPageBitmapWithMatrix(bitmap, page, matrix, clipping, flags);
    printf("INFO: Rendered in %.02f ms\n", curTimeMs()-startMs);
}
