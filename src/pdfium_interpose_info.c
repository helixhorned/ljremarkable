#define _GNU_SOURCE

#include "pdfium/fpdfview.h"
#include "interpose/include/interpose.h"

#include <stdio.h>
#include <time.h>

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
    printf("      dimensions => %f x %f\n", FPDF_GetPageWidth(page), FPDF_GetPageHeight(page));
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
    printf("INFO: Render: m = [%f %f 0;  bitmap=%p, flags=0x%x\n"
           "                   %f %f 0;\n"
           "                   %f %f 1]\n"
           "              clip = (%f, %f)--(%f, %f)\n",
           m->a, m->b, (void *)bitmap, flags, m->c, m->d, m->e, m->f,
           c->left, c->top, c->right, c->bottom);

    const Millisecs startMs = curTimeMs();
    Real__FPDF_RenderPageBitmapWithMatrix(bitmap, page, matrix, clipping, flags);
    printf("INFO: Rendered in %.02f ms\n", curTimeMs()-startMs);
}