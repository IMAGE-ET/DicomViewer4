DicomViewer4
============

DicomViewer4 is a multiplatform/multi-modality DicomViewer written in
FreePascal with the fpGui toolkit. 

The project has two goals, first, it has to be easily portable to other
platforms/languages (JavaScript/HTML5), so, its code must be clean and
correct, also it shouldn't require external dependencies (currently it uses
dcmtk, but this can change if you help the project). Second, it must run fast,
specially when dealing with big studies such actual CT scans and XA
(angiography) images, even on low-end computers.

Dependencies
------------

For loading and decompressing dicom files, the viewer uses a shared library,
libdicomutils.so (or .dll) based on dcmtk (www.dcmtk.org).

