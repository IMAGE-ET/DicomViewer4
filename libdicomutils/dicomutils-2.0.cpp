#include <dcmtk/config/osconfig.h>
#include <dcmtk/oflog/oflog.h>
#include <dcmtk/oflog/nullap.h>
#include <cstring>
#include "string.h"
#include "dcmtk/ofstd/ofstream.h"
#include "dcmtk/dcmdata/dcmetinf.h"
#include <dcmtk/dcmnet/dfindscu.h>
#include "dcmtk/dcmdata/dcdict.h"
#include <dcmtk/ofstd/ofbmanip.h>
#include <dcmtk/ofstd/ofstring.h>
#include <dcmtk/ofstd/ofconsol.h>
#include <dcmtk/dcmdata/dcdeftag.h>
#include <dcmtk/dcmdata/dcfilefo.h>
#include <dcmtk/dcmdata/dcistrmb.h>
#include <dcmtk/dcmimgle/dcmimage.h>
#include <dcmtk/dcmimage/diregist.h> /* include to support color images */
#include <dcmtk/dcmjpeg/djdecode.h>  /* for jpeg decoder */
#include <dcmtk/dcmdata/dcrledrg.h>
#include <dcmtk/dcmdata/dcdicdir.h>

#include <iostream>
using namespace std;

#define DCM_PatientsName                         DcmTagKey(0x0010, 0x0010)
#define DCM_PatientsBirthDate                    DcmTagKey(0x0010, 0x0030)
#define DCM_PatientsBirthTime                    DcmTagKey(0x0010, 0x0032)
#define DCM_PatientsSex                          DcmTagKey(0x0010, 0x0040)
#define DCM_PatientsBirthName                    DcmTagKey(0x0010, 0x1005)
#define DCM_PatientsAge                          DcmTagKey(0x0010, 0x1010)
#define DCM_PatientsSize                         DcmTagKey(0x0010, 0x1020)
#define DCM_PatientsWeight                       DcmTagKey(0x0010, 0x1030)
#define DCM_PatientsAddress                      DcmTagKey(0x0010, 0x1040)
#define DCM_ReferringPhysiciansName              DcmTagKey(0x0008, 0x0090)
#define DCM_PerformingPhysiciansName             DcmTagKey(0x0008, 0x1050)
#define DCM_RetrieveAETitle                      DcmTagKey(0x0008, 0x0054)

typedef void (* CFindCallBack)(const char *, const void *);
typedef void (* DICOMDIROnStudy)(const char * /* StudyDescription */, const char * /* StudyInstanceUID */, const char * /* Accession */, const char * /* StudyDate */, void *);
typedef void (* DICOMDIROnPatient)(const char * /* PatientName */, const char * /* PatientId */,  void *);
typedef void (* DICOMDIROnSerie)(const char * /* SeriesDescription */, const char * /* SeriesNumber */, const char * /* Modality */, void *);
typedef void (* DICOMDIROnImage)(const char * /* ReferencedFileID */, const char * /* InstanceNumber */, void *);


class DcmFindSCUmyCallback: public DcmFindSCUCallback
{
public:
    DcmFindSCUmyCallback(void * caller){
        FCaller = caller;
    }

  virtual ~DcmFindSCUmyCallback() {}

  void setCallBack(CFindCallBack ACallBack)
  {
      FCallBack = ACallBack;
  }

  virtual void callback(
        T_DIMSE_C_FindRQ *request,
        int responseCount,
        T_DIMSE_C_FindRSP *rsp,
        DcmDataset *responseIdentifiers)
  {

      DcmStack stack;
      DcmObject *dobject = NULL;
      DcmElement *delem = NULL;
      OFString lValue;
      OFString lResult;
      OFString lTmp;
      OFCondition status = responseIdentifiers->nextObject(stack, OFTrue);

      while (status.good())
      {
        dobject = stack.top();
        DcmTagKey lTagKey(dobject->getTag().getXTag());
        lTmp = lTagKey.toString();

        delem = (DcmElement *)dobject;
        if(delem->isLeaf())
        {
          delem->getOFStringArray(lValue);
          lResult += lTagKey.toString();
          DcmTag lTag(lTagKey);
          lResult += lTag.getTagName();
          lResult += "=";
          lResult += lValue.c_str();
          lResult += "\n";
        }
        status = responseIdentifiers->nextObject(stack, OFTrue);
      }
      FCallBack(lResult.c_str(), FCaller);
  }

private:
  CFindCallBack FCallBack;
  void * FCaller;
};


class DicomImageHandler
{
  public:
    DicomImageHandler();
    ~DicomImageHandler();
    void rotate(int angle);
    void setDicomImage(DicomImage * image);
    void setDataset(DcmDataset * dataset);
    void setDepth(int depth);
    void setSamplesPerPixel(int samplesPerPixel);
    void setZoomFactor(double zoomFactor);
    void setPolarity(EP_Polarity polarity);
    void setCurrentFrame(int frame);
    EP_Polarity getPolarity();
    int getDensity(int x, int y);
    int getDepth();
    int getSamplesPerPixel();
    int getFrameCount();
    int getCurrentFrame();
    int getAngle();
    double getZoomFactor();
    DicomImage * getDicomImage();
    DicomImage * getZoomedImage();
    DcmDataset * getDataset();
    Uint8 * getBuffer();
    unsigned long getLength();
  private:
    DicomImage * m_dicomimage;
    DicomImage * m_zoomedImage;
    DcmDataset * m_dcmdataset;
    double m_zoomFactor;
    int m_angle;
    int m_depth;
    int m_samplesPerPixel;
    int m_currentFrame;
};

DicomImageHandler::DicomImageHandler()
{
  m_zoomFactor = 1.0;
  m_currentFrame = 1;
  m_zoomedImage = NULL;
  m_angle = 0;
}

DicomImageHandler::~DicomImageHandler()
{
    delete m_zoomedImage;
    delete m_dicomimage;
    delete m_dcmdataset;
    m_dicomimage = NULL;
    m_zoomedImage = NULL;
}

int DicomImageHandler::getAngle()
{
    return m_angle;
}

void DicomImageHandler::setCurrentFrame(int frame)
{
    m_currentFrame = frame;
}

int DicomImageHandler::getFrameCount()
{
    DicomImage * lImage = getDicomImage();
    return lImage->getFrameCount();
}

int DicomImageHandler::getCurrentFrame()
{
    return m_currentFrame;
}


void DicomImageHandler::rotate(int angle)
{
  m_dicomimage->rotateImage(angle);
  m_angle = angle;
}

void DicomImageHandler::setPolarity(EP_Polarity polarity)
{
  DicomImage * lImage = getDicomImage();
  lImage->setPolarity(polarity);
}

EP_Polarity DicomImageHandler::getPolarity()
{
  DicomImage * lImage = getDicomImage();
  return lImage->getPolarity();
}

void DicomImageHandler::setZoomFactor(double zoomFactor)
{
  m_zoomFactor = zoomFactor;
  /* Se crea una versión escalada de la imágen original */
  m_zoomedImage = getDicomImage()->createScaledImage(zoomFactor, 0, 1);
}

void DicomImageHandler::setDepth(int depth)
{
  m_depth = depth;
}

double DicomImageHandler::getZoomFactor()
{
  return m_zoomFactor;
}

int DicomImageHandler::getDepth()
{
  return m_depth;
}

void DicomImageHandler::setSamplesPerPixel(int samplesPerPixel)
{
  m_samplesPerPixel = samplesPerPixel;
}

int DicomImageHandler::getSamplesPerPixel()
{
  return m_samplesPerPixel;
}

int DicomImageHandler::getDensity(int x, int y)
{
    int iPos = x + y * m_dicomimage->getWidth();
    OFString str;
    const DiPixel *dmp = NULL;
    dmp = m_dicomimage->getInterData();
    void *pixelData = NULL;
    pixelData = (void *)dmp->getData();

    if(NULL == dmp)
    {
        return 0;
    }

    int result = 0;

    EP_Representation rep = dmp->getRepresentation();
    switch(rep)
    {
    case EPR_Sint32:
        {
            Sint32 *pixelNew1 = ((Sint32*)pixelData+iPos);
            result = *pixelNew1;
            break;
        }
    case EPR_Uint32:
        {
            Uint32 *pixelNew2 = ((Uint32*)pixelData+iPos);
            result = *pixelNew2;
            break;
        }
    case EPR_Sint16:
        {
            Sint16 *pixelNew3 = ((Sint16*)pixelData+iPos);
            result = *pixelNew3;
            break;
        }
    case EPR_Uint16:
        {
            Uint16 *pixelNew4 = ((Uint16*)pixelData+iPos);
            result = *pixelNew4;
            break;
        }
    case EPR_Uint8:
        {
            Uint8 *pixelNew5 = ((Uint8*)pixelData+iPos);
            result = *pixelNew5;
            break;
        }
    default:
        {
            Sint8 *pixelNew6 = ((Sint8*)pixelData+iPos);
            result = *pixelNew6;
            break;
        }
    }

    return result;
}

void DicomImageHandler::setDataset(DcmDataset * dataset)
{
    m_dcmdataset = (DcmDataset *)dataset->clone();
}

void DicomImageHandler::setDicomImage(DicomImage * image)
{
  m_dicomimage = image;
}

DcmDataset * DicomImageHandler::getDataset()
{
  return m_dcmdataset;
}

DicomImage * DicomImageHandler::getDicomImage()
{
  return m_dicomimage;
}

DicomImage * DicomImageHandler::getZoomedImage()
{
  return m_zoomedImage;
}

void getDataSet(void * imagen, void * &dataset)
{
  DicomImageHandler * imgHandler = (DicomImageHandler *)imagen;
  std::stringbuf sb;
  std::ostream stream(&sb);
  imgHandler->getDataset()->print(stream);
  dataset = stream;
}

void getWindowLevel(void * imagen, double &center, double &width)
{
  DicomImageHandler * imgHandler = (DicomImageHandler *)imagen;
  imgHandler->getDicomImage()->getWindow(center, width);
}

void applyWindowLevel(void * imagen, double center, double width)
{
  DicomImageHandler * imgHandler = (DicomImageHandler *)imagen;
  imgHandler->getDicomImage()->setWindow(center, width);
  imgHandler->getZoomedImage()->setWindow(center, width);
}

void getDimensions(void * image, int &width, int &height)
{
    DicomImageHandler * imgHandler = (DicomImageHandler *)image;
    width = imgHandler->getDicomImage()->getWidth();
    height = imgHandler->getDicomImage()->getHeight();
}

// reemplaza el pixeldata 
int replaceDicomPixelData(void * imagen, void * buffer, unsigned long bufSize, char * tmpFile)
{
   DicomImageHandler * imgHandler = (DicomImageHandler *)imagen;
   int lResult = 0;
   if(imgHandler->getDicomImage()->getStatus() == EIS_Normal)
   {
     DcmDataset * lDataset = imgHandler->getDataset();
     DicomImage * lImage = imgHandler->getDicomImage();
     int cols = lImage->getHeight();
     int rows = lImage->getWidth();
     /* Image Pixel Module */
     Uint16 lSamples;
     lDataset->findAndGetUint16(DCM_SamplesPerPixel, lSamples);
     Uint16 lBitsAllocated;
     lDataset->findAndGetUint16(DCM_BitsAllocated, lBitsAllocated);
     Uint16 lBitsStored;
     lDataset->findAndGetUint16(DCM_BitsStored, lBitsStored);
     Uint16 lHighBit;
     lDataset->findAndGetUint16(DCM_HighBit, lHighBit);
     Uint16 lPixelRepresentation;
     lDataset->findAndGetUint16(DCM_PixelRepresentation, lPixelRepresentation);
     OFString lRescaleIntercept;
     lDataset->findAndGetOFString(DCM_RescaleIntercept, lRescaleIntercept);
     OFString lRescaleSlope;
     lDataset->findAndGetOFString(DCM_RescaleSlope, lRescaleSlope);

     OFString lPhotoInt;
     lDataset->findAndGetOFString(DCM_PhotometricInterpretation, lPhotoInt);
     OFString lStudyDate;
     lDataset->findAndGetOFString(DCM_StudyDate, lStudyDate);
     OFString lStudyTime;
     lDataset->findAndGetOFString(DCM_StudyTime, lStudyTime);
     OFString lAccession;
     lDataset->findAndGetOFString(DCM_AccessionNumber, lAccession);
     OFString lModality;
     lDataset->findAndGetOFString(DCM_Modality, lModality);
     OFString lPatientId;
     lDataset->findAndGetOFString(DCM_PatientID, lPatientId);
     OFString lPatientName;
     lDataset->findAndGetOFString(DCM_PatientsName, lPatientName);
     OFString lPatientsBirthDate;
     lDataset->findAndGetOFString(DCM_PatientsBirthDate, lPatientsBirthDate);
     OFString lPatientsBirthTime;
     lDataset->findAndGetOFString(DCM_PatientsBirthTime, lPatientsBirthTime);
     OFString lPatientsSex;
     lDataset->findAndGetOFString(DCM_PatientsSex, lPatientsSex);
     OFString lStudyInstanceUID;
     lDataset->findAndGetOFString(DCM_StudyInstanceUID, lStudyInstanceUID);
     OFString lSeriesInstanceUID;
     lDataset->findAndGetOFString(DCM_SeriesInstanceUID, lSeriesInstanceUID);
     OFString lStudyID;
     lDataset->findAndGetOFString(DCM_StudyID, lStudyID);
     OFString lSeriesNumber;
     lDataset->findAndGetOFString(DCM_SeriesNumber, lSeriesNumber);
     OFString lInstanceNumber;
     lDataset->findAndGetOFString(DCM_InstanceNumber, lInstanceNumber);
     OFString lPixelSpacing;
     lDataset->findAndGetOFStringArray(DCM_PixelSpacing, lPixelSpacing);
     OFString lWindowWidth;
     lDataset->findAndGetOFString(DCM_WindowWidth, lWindowWidth);
     OFString lWindowCenter;
     lDataset->findAndGetOFString(DCM_WindowCenter, lWindowCenter);
     Uint16 lPixelPaddingValue;
     lDataset->findAndGetUint16(DCM_PixelPaddingValue, lPixelPaddingValue);
     OFString lSOPClassUID;
     lDataset->findAndGetOFString(DCM_SOPClassUID, lSOPClassUID);
 
     // Generamos una imagen nueva
     DcmFileFormat fileformat;
     DcmDataset *dataset = fileformat.getDataset();
     dataset->putAndInsertUint16(DCM_SamplesPerPixel, lSamples);
     // por ahora con RGB no trabajamos
     dataset->putAndInsertString(DCM_PhotometricInterpretation, "MONOCHROME2");
     dataset->putAndInsertUint16(DCM_Rows, cols);
     dataset->putAndInsertUint16(DCM_Columns, rows);
     dataset->putAndInsertUint16(DCM_BitsAllocated, lBitsAllocated);
     dataset->putAndInsertUint16(DCM_BitsStored,lBitsStored);
     dataset->putAndInsertUint16(DCM_HighBit, lHighBit);
     dataset->putAndInsertString(DCM_StudyDate, lStudyDate.c_str());
     dataset->putAndInsertString(DCM_StudyTime, lStudyTime.c_str());
     dataset->putAndInsertString(DCM_AccessionNumber, lAccession.c_str());
     dataset->putAndInsertString(DCM_Modality, lModality.c_str());
     dataset->putAndInsertString(DCM_PatientID, lPatientId.c_str());
     dataset->putAndInsertString(DCM_PatientsName, lPatientName.c_str());
     dataset->putAndInsertString(DCM_PatientsBirthDate, lPatientsBirthDate.c_str());
     dataset->putAndInsertString(DCM_PatientsBirthTime, lPatientsBirthTime.c_str());
     dataset->putAndInsertString(DCM_PatientsSex, lPatientsSex.c_str());
     dataset->putAndInsertString(DCM_StudyInstanceUID, lStudyInstanceUID.c_str());
     dataset->putAndInsertString(DCM_SeriesInstanceUID, lSeriesInstanceUID.c_str());
     dataset->putAndInsertString(DCM_StudyID, lStudyID.c_str());
     dataset->putAndInsertString(DCM_SeriesNumber, lSeriesNumber.c_str());
     dataset->putAndInsertString(DCM_InstanceNumber, lInstanceNumber.c_str());
     dataset->putAndInsertOFStringArray(DCM_PixelSpacing, lPixelSpacing.c_str());
     dataset->putAndInsertString(DCM_WindowWidth, lWindowWidth.c_str());
     dataset->putAndInsertString(DCM_WindowCenter, lWindowCenter.c_str());
     dataset->putAndInsertUint16(DCM_PixelPaddingValue, lPixelPaddingValue);
     lPixelRepresentation = 0;
     dataset->putAndInsertUint16(DCM_PixelRepresentation, lPixelRepresentation);
     dataset->putAndInsertString(DCM_RescaleIntercept, lRescaleIntercept.c_str());
     dataset->putAndInsertString(DCM_RescaleSlope, lRescaleSlope.c_str());
     dataset->putAndInsertString(DCM_SOPClassUID, lSOPClassUID.c_str());

     if(lBitsAllocated == 16)
     {
       if(dataset->putAndInsertUint16Array(DCM_PixelData, (const Uint16 *)buffer, rows * cols) == EC_Normal) 
       { 
          lResult = 1;
       }
     }
     if(lBitsAllocated == 8)
     {
       if(dataset->putAndInsertUint8Array(DCM_PixelData, (const Uint8 *)buffer, rows * cols) == EC_Normal) 
       { 
          lResult = 1;
       }
     }
  
     if(lResult == 1)
     {
       DcmFileFormat ff(dataset); 
       ff.saveFile(
        tmpFile,
        EXS_LittleEndianExplicit,
        EET_ExplicitLength,
        EGL_recalcGL,
        EPD_noChange,
        0,
        0,
        EWM_createNewMeta);

       //dataset->saveFile(tmpFile, EXS_LittleEndianImplicit);//,
       //  EET_UndefinedLength, EGL_withoutGL); 
    }

   }
   return lResult;
}

// Carga la imagen original completa en un buffer.
int getImageBuffer(void * imagen, void * &buffer, unsigned long& bufSize, bool onlyPixelData)
{
   DicomImageHandler * imgHandler = (DicomImageHandler *)imagen;
   int lResult = 0;
   if(imgHandler->getDicomImage()->getStatus() == EIS_Normal)
   {
     if(!onlyPixelData)
     {
       double lZoom = imgHandler->getZoomFactor();
       DicomImage * lTmpImage = imgHandler->getDicomImage()->createDicomImage(imgHandler->getCurrentFrame(), 1);
       DicomImage * lScaledImage = lTmpImage->createScaledImage(lZoom, 0, 1);
       bufSize = lScaledImage->getOutputDataSize();
       buffer = new Uint8[bufSize];
       if(lScaledImage->getOutputData((void *)(buffer), bufSize, 8, 0))
         lResult = 1;
       delete lScaledImage;
       delete lTmpImage;
     } else {
        const DiPixel * interdata;
        if(imgHandler->getDicomImage()->isMonochrome()){
          interdata = imgHandler->getDicomImage()->getInterData();
          EP_Representation rep = interdata->getRepresentation();
          switch(rep)
          {
          case EPR_Sint32:
              {
                  bufSize = interdata->getCount() * sizeof(Sint32);
                  buffer = new Sint32[bufSize];
                  break;
              }
          case EPR_Uint32:
              {
                  bufSize = interdata->getCount() * sizeof(Uint32);
                  buffer = new Uint32[bufSize];
                  break;
              }
          case EPR_Sint16:
              {
                  bufSize = interdata->getCount() * sizeof(Sint16);
                  buffer = new Sint16[bufSize];
                  break;
              }
          case EPR_Uint16:
              {
                  bufSize = interdata->getCount() * sizeof(Uint16);
                  buffer = new Uint16[bufSize];
                  break;
              }
          case EPR_Uint8:
              {
                  bufSize = interdata->getCount() * sizeof(Uint8);
                  buffer = new Uint8[bufSize];
                  break;
              }
          default:
              {
                  bufSize = interdata->getCount() * sizeof(Sint8);
                  buffer = new Sint8[bufSize];
                  break;
              }
          }
          memcpy((void *)buffer, interdata->getData(), bufSize);
        } else {
          bufSize = imgHandler->getDicomImage()->getOutputDataSize();
          buffer = new Uint8[bufSize];
          imgHandler->getDicomImage()->getOutputData((void *)(buffer), bufSize, 8, 0);
        }
        lResult = 1;
     }

   }
   return lResult;
}

// Carga una región de la imagen original (mucho más rápido)
int getImageBufferEx(void * imagen, void * &buffer, unsigned long& bufSize, double left, double top, double &width, double &height, double zoom, bool onlyPixelData)
{
   DicomImageHandler * imgHandler = (DicomImageHandler *)imagen;
   int lResult = -1;
   if(imgHandler->getDicomImage()->getStatus() == EIS_Normal)
   {
     double lZoom = imgHandler->getZoomFactor();

     DicomImage * clippedImage = NULL;
     DicomImage * lNewImage = NULL;

     if(zoom == -1){
        DicomImage * lOrigImage = imgHandler->getZoomedImage();
        clippedImage = lOrigImage->createClippedImage(left, top, width, height);
     }
     else
     {
        DicomImage * lOrigImage = imgHandler->getDicomImage();
        lNewImage = lOrigImage->createClippedImage(left, top, width, height);
        clippedImage = lNewImage->createScaledImage(zoom, 0, 1);
     }
         
     if(clippedImage == NULL) 
       return -1;

     width = clippedImage->getWidth();
     height = clippedImage->getHeight();
     if(!onlyPixelData)
     {
       bufSize = clippedImage->getOutputDataSize();
       buffer = new Uint8[bufSize];
       if(clippedImage->getOutputData((void *)(buffer), bufSize, 8))
         lResult = 1;
     } else {
        const DiPixel * interdata = clippedImage->getInterData();
        EP_Representation rep = interdata->getRepresentation();
        switch(rep)
        {
        case EPR_Sint32:
            {
                bufSize = interdata->getCount() * sizeof(Sint32);
                buffer = new Sint32[bufSize];
                break;
            }
        case EPR_Uint32:
            {
                bufSize = interdata->getCount() * sizeof(Uint32);
                buffer = new Uint32[bufSize];
                break;
            }
        case EPR_Sint16:
            {
                bufSize = interdata->getCount() * sizeof(Sint16);
                buffer = new Sint16[bufSize];
                break;
            }
        case EPR_Uint16:
            {
                bufSize = interdata->getCount() * sizeof(Uint16);
                buffer = new Uint16[bufSize];
                break;
            }
        case EPR_Uint8:
            {
                bufSize = interdata->getCount() * sizeof(Uint8);
                buffer = new Uint8[bufSize];
                break;
            }
        default:
            {
                bufSize = interdata->getCount() * sizeof(Sint8);
                buffer = new Sint8[bufSize];
                break;
            }
        }
        memcpy((void *)buffer, interdata->getData(), bufSize);
        lResult = 1;
     }
     delete clippedImage;
  
     if(lNewImage != NULL)
        delete lNewImage;
   }
   return lResult;
}

bool OpenImageFromFF(DcmFileFormat * fileFormat, void * &imagen)
{
    bool lResult = false;
    unsigned long fstart = 0;
    unsigned long fcount = 0;
    E_TransferSyntax xfer = fileFormat->getDataset()->getOriginalXfer();
    DicomImage * image = new DicomImage(fileFormat, EXS_Unknown, CIF_MayDetachPixelData||CIF_IgnoreModalityTransformation, fstart, fcount);
    if(EIS_Normal == image->getStatus())
    {
       int lWindows = image->getWindowCount();
       if(lWindows > 0) 
         image->setWindow(0);
       else
         image->setMinMaxWindow(0);
       DicomImageHandler * imgHandler = new DicomImageHandler();
       imgHandler->setDataset(fileFormat->getDataset());
       imgHandler->setDicomImage(image);
       imgHandler->setDepth(image->getDepth());
       if(image->isMonochrome())
         imgHandler->setSamplesPerPixel(1);
       else
         imgHandler->setSamplesPerPixel(3);
        imgHandler->setPolarity(image->getPolarity());
       imagen = imgHandler;
       lResult = true;
    }


    return lResult;
}

void getTags(void * imagen, char * &output)
{
    DicomImageHandler * imgHandler = (DicomImageHandler *)imagen;
    std::stringbuf sb;
    std::ostream out(&sb);
    // eliminamos el tag PixelData
    imgHandler->getDataset()->remove(DCM_PixelData);
    imgHandler->getDataset()->print(out);

    int len = strlen(sb.str().c_str()) + 1;
    output = new char[len];
    std::strcpy(output, sb.str().c_str());
}

void getDensity(void * imagen, int x, int y, int &density)
{
    DicomImageHandler * imgHandler = (DicomImageHandler *)imagen;
    density = imgHandler->getDensity(x, y);
}


void getRectRoi(void * imagen, int x, int y, int w, int h, double &median, double &stddev)
{
    DicomImageHandler * imgHandler = (DicomImageHandler *)imagen;
    DicomImage * tmpDicomImg = imgHandler->getDicomImage()->createClippedImage(x, y, w, h);
    const DiPixel *dmp = NULL;
    dmp = tmpDicomImg->getInterData();
    void *pixelData = NULL;
    pixelData = (void *)dmp->getData();

    if(NULL == dmp)
    {
        return;
    }

    EP_Representation rep = dmp->getRepresentation();

    int iPos = 0;
    int lRes = 0;
    int lTotal = dmp->getCount();
    for(iPos = 0; iPos < lTotal; iPos++)
    {
        switch(rep)
        {
        case EPR_Sint32:
            {
                Sint32 *pixelNew1 = ((Sint32*)pixelData+iPos);
                lRes = lRes + *pixelNew1;
                break;
            }
        case EPR_Uint32:
            {
                Uint32 *pixelNew2 = ((Uint32*)pixelData+iPos);
                lRes = lRes +*pixelNew2;
                break;
            }
        case EPR_Sint16:
            {
                Sint16 *pixelNew3 = ((Sint16*)pixelData+iPos);
                lRes = lRes +*pixelNew3;
                break;
            }
        case EPR_Uint16:
            {
                Uint16 *pixelNew4 = ((Uint16*)pixelData+iPos);
                lRes = lRes +*pixelNew4;
                break;
            }
        case EPR_Uint8:
            {
                Uint8 *pixelNew5 = ((Uint8*)pixelData+iPos);
                lRes = lRes +*pixelNew5;
                break;
            }
        default:
            {
                Sint8 *pixelNew6 = ((Sint8*)pixelData+iPos);
                lRes = lRes +*pixelNew6;
                break;
            }
        }

    }
    median = lRes / lTotal;
    delete(tmpDicomImg);
}


bool openFile(char * file, void * &imagen)
{
  DcmFileFormat * fileFormat = new DcmFileFormat();
  OFCondition cond = fileFormat->loadFile(file);
  bool lReturn = false;
  if(cond.good())
  {
    lReturn =  OpenImageFromFF(fileFormat, imagen);
  }
  delete fileFormat;
  return lReturn;
}

bool openHeaderFromFile(char * file, char * &output)
{
  bool lResult = false;
  DcmFileFormat fileFormat;
  OFCondition cond = fileFormat.loadFile(file, EXS_Unknown, EGL_noChange, 2048);
  if(cond.good())
  {
    std::stringbuf sb;
    std::ostream out(&sb);
    fileFormat.getDataset()->print(out);
    int len = strlen(sb.str().c_str()) + 1;
    output = new char[len];
    std::strcpy(output, sb.str().c_str());
    lResult = true;
  }
  return lResult;
}


bool openHeaderFromBuffer(const void * buffer, ulong buflen, char * &output)
{
  bool lResult = false;
  DcmInputBufferStream dataBuf;
  dataBuf.setBuffer(buffer, buflen);
  dataBuf.setEos();

  DcmFileFormat fileFormat;
  DcmTagKey tag(0x2050, 0x0010); // PresentationLUTSequence
  dcmStopParsingAfterElement.set(tag);
  fileFormat.transferInit();
  OFCondition cond = fileFormat.read(dataBuf);
  fileFormat.transferEnd();
  dcmStopParsingAfterElement.set(DCM_UndefinedTagKey);
  if(cond.good())
  {
    std::stringbuf sb;
    std::ostream out(&sb);
    fileFormat.getDataset()->print(out);
    int len = strlen(sb.str().c_str()) + 1;
    output = new char[len];
    std::strcpy(output, sb.str().c_str());
    lResult = true;
  }

  return lResult;
}


bool openBuffer(const void * buffer, ulong buflen, void * &imagen)
{
  bool lResult = false;
  DcmInputBufferStream dataBuf;
  dataBuf.setBuffer(buffer, buflen);
  dataBuf.setEos();

  DcmFileFormat * fileFormat = new DcmFileFormat();
  fileFormat->transferInit();
  OFCondition cond = fileFormat->read(dataBuf);
  fileFormat->transferEnd();
  if(cond.good())
  {
    lResult = OpenImageFromFF(fileFormat, imagen);
  }

  delete fileFormat;

  return lResult;
}

void setZoom(void * imagen, double zoom)
{
     DicomImageHandler * imgHandler = (DicomImageHandler *)imagen;
     imgHandler->setZoomFactor(zoom);
}

void rotate(void * imagen, int angle)
{
     DicomImageHandler * imgHandler = (DicomImageHandler *)imagen;
     imgHandler->rotate(angle);
}

double getZoom(void * imagen)
{
     DicomImageHandler * imgHandler = (DicomImageHandler *)imagen;
     return imgHandler->getZoomFactor();
}

int getDepth(void * imagen)
{
     DicomImageHandler * imgHandler = (DicomImageHandler *)imagen;
     return imgHandler->getDepth();
}

int getSamplesPerPixel(void * imagen)
{
     DicomImageHandler * imgHandler = (DicomImageHandler *)imagen;
     return imgHandler->getSamplesPerPixel();
}

void saveToFile(void * imagen, char * file)
{
    DicomImageHandler * imgHandler = (DicomImageHandler *)imagen;
    DcmDataset lDataset( * imgHandler->getDataset() ); 
    imgHandler->getDicomImage()->writeImageToDataset(lDataset);
    DcmFileFormat ff(&lDataset); 
    ff.saveFile(
      file,
      EXS_LittleEndianExplicit,
      EET_ExplicitLength,
      EGL_recalcGL,
      EPD_noChange,
      0,
      0,
      EWM_createNewMeta);
}

EP_Polarity getPolarity(void * imagen)
{
     DicomImageHandler * imgHandler = (DicomImageHandler *)imagen;
     return imgHandler->getPolarity();
}

void setPolarity(void * imagen, EP_Polarity polarity)
{
     DicomImageHandler * imgHandler = (DicomImageHandler *)imagen;
     imgHandler->setPolarity(polarity);
}

void setDicomCurrentFrame(void * imagen, int frame)
{
    DicomImageHandler * imgHandler = (DicomImageHandler *)imagen;
    imgHandler->setCurrentFrame(frame);
}

int getDicomFrameCount(void * imagen)
{
    DicomImageHandler * imgHandler = (DicomImageHandler *)imagen;
    return imgHandler->getFrameCount();
}

bool copyImage(void * imagen, void * &imgCopy)
{
  DicomImageHandler * imgHandler = (DicomImageHandler *)imagen;
  DicomImage * image = imgHandler->getDicomImage()->createDicomImage();
  int lWindows = image->getWindowCount();
  if(lWindows > 0) 
    image->setWindow(0);
  else
    image->setMinMaxWindow(0);
  DicomImageHandler * newImgHandler = new DicomImageHandler();
  DcmDataset * lDataset = new DcmDataset(*imgHandler->getDataset());
  newImgHandler->setDataset(lDataset);
  newImgHandler->setDicomImage(image);
  newImgHandler->setDepth(image->getDepth());
  if(image->isMonochrome())
    newImgHandler->setSamplesPerPixel(1);
  else
    newImgHandler->setSamplesPerPixel(3);
  newImgHandler->setPolarity(image->getPolarity());
  imgCopy = newImgHandler;

  return true;
}

extern "C" {
  void geDicomtDataSet(void * imagen, void * &dataset)
  {
    getDataSet(imagen, dataset);
  }

  bool fileIsDicom(char * fname)
  {
    bool ok = false;
    FILE* f = NULL;

    f = fopen(fname, "rb");
    if (f == 0) {
        ok = false;
    } else {
        char signature[4];
        if ((fseek(f, DCM_PreambleLen, SEEK_SET) < 0) ||
            (fread(signature, 1, DCM_MagicLen, f) != DCM_MagicLen)) {
            ok = false;
        } else if (strncmp(signature, DCM_Magic, DCM_MagicLen) != 0) {
            ok = false;
        } else {
            /* looks ok */
            ok = true;
        }
        fclose(f);
    }
    return ok;
  }



  bool openDicomHeaderFromBuffer(const void * buffer, ulong buflen, char * &output)
  {
    return openHeaderFromBuffer(buffer, buflen, output);
  }

  bool openDicomHeaderFromFile(char * file, char * &output)
  {
    return openHeaderFromFile(file, output);
  }

  bool openDicomDir(char * file, char * &output)
  {
    DcmFileFormat fileFormat;
    OFCondition cond = fileFormat.loadFile(file);
    if(cond.good())
    {
      std::stringbuf sb;
      std::ostream out(&sb);
      // eliminamos el tag PixelData
      fileFormat.getDataset()->remove(DCM_PixelData);
      fileFormat.getDataset()->print(out);
      int len = strlen(sb.str().c_str()) + 1;
      output = new char[len];
      std::strcpy(output, sb.str().c_str());
      return true; 
    }
    else
     return false;
  }

  bool openDicomFile(char * file, void  * &imagen)
  {
    return openFile(file, imagen);
  }

  bool openDicomBuffer(const void * buffer, unsigned long bufsize, void  * &imagen)
  {
    return openBuffer(buffer, bufsize, imagen);
  }

  void bmpToDicom(const long * bmpBuffer, unsigned long bmpBufSize, 
    unsigned long aRows, unsigned long aCols, unsigned long aBitsAllocated, 
    unsigned long aBitsStored, unsigned long aHighBit,
    unsigned long aSamplesPerPixel,
    const char * aPhotometricInterpretation,
    const char * AFile)
  {
    // bmpBufSize es width*height*3 (el 3 es de R,G,B)
    DcmFileFormat fileformat;
    char uid[100]; 
    DcmDataset *dataset = fileformat.getDataset();
    dataset->putAndInsertString(DCM_SOPClassUID, UID_SecondaryCaptureImageStorage);
    dataset->putAndInsertString(DCM_SOPInstanceUID, dcmGenerateUniqueIdentifier(uid, SITE_INSTANCE_UID_ROOT));

    dataset->putAndInsertString(DCM_StudyInstanceUID,dcmGenerateUniqueIdentifier(uid, SITE_INSTANCE_UID_ROOT));
    dataset->putAndInsertString(DCM_StudyDate,"20051231");
    dataset->putAndInsertString(DCM_StudyTime,"20051231");
    dataset->putAndInsertString(DCM_StudyID,"1234567");

    dataset->putAndInsertString(DCM_SeriesInstanceUID,dcmGenerateUniqueIdentifier(uid, SITE_INSTANCE_UID_ROOT));
    dataset->putAndInsertString(DCM_SeriesNumber,"0");

    if(aBitsStored >= 8)
      dataset->putAndInsertString(DCM_Modality,"SC");
    else
      dataset->putAndInsertString(DCM_Modality,"MG");

    dataset->putAndInsertString(DCM_ConversionType,"DV");
    dataset->putAndInsertString(DCM_AccessionNumber,"123456");

    dataset->putAndInsertString(DCM_Manufacturer,"GRIENSU");
    dataset->putAndInsertString(DCM_InstitutionName,"GRIENSU");

    dataset->putAndInsertString(DCM_PatientsName, "Print^Composer");
    dataset->putAndInsertString(DCM_PatientID,"GRIENSUID");
    dataset->putAndInsertString(DCM_PatientsBirthDate,"20051231");
    dataset->putAndInsertString(DCM_PatientsSex,"M");

    dataset->putAndInsertUint16(DCM_SamplesPerPixel, aSamplesPerPixel);
    dataset->putAndInsertString(DCM_PhotometricInterpretation, aPhotometricInterpretation);
    dataset->putAndInsertUint16(DCM_BitsAllocated, aBitsAllocated);
    dataset->putAndInsertUint16(DCM_BitsStored, aBitsStored);
    dataset->putAndInsertUint16(DCM_HighBit, aHighBit);
    dataset->putAndInsertUint16(DCM_PixelRepresentation,0);
    dataset->putAndInsertUint16(DCM_PlanarConfiguration,0);
    dataset->putAndInsertUint16(DCM_Rows,aRows);
    dataset->putAndInsertUint16(DCM_Columns,aCols);

    if(aBitsStored >= 8)
      dataset->putAndInsertUint8Array(DCM_PixelData, (Uint8*)bmpBuffer, bmpBufSize);
    else
      dataset->putAndInsertUint16Array(DCM_PixelData, (Uint16*)bmpBuffer, bmpBufSize);

    OFCondition status = fileformat.saveFile(AFile, EXS_LittleEndianImplicit,EET_UndefinedLength,EGL_recalcGL); 
  }

  void registerCodecs()
  {
    // Registramos codecs de descompresión JPeg
    DJDecoderRegistration::registerCodecs();
  }

  void unRegisterCodecs()
  {
    // Desregistramos codecs
    DJDecoderRegistration::cleanup();
  }

  bool copyDicomImage(void * image, void * &copy)
  {
    return copyImage(image, copy);
  }

  int replacePixelData(void * image, void * buffer, unsigned long bufSize, char * tmpFile)
  {
    return replaceDicomPixelData(image, buffer, bufSize, tmpFile);
  }

  int getDicomImageBuffer(void * image, void * &buffer, unsigned long& bufSize, bool onlyPixelData)
  {
    return getImageBuffer(image, buffer, bufSize, onlyPixelData);
  }

  int getDicomImageBufferEx(void * image, void * &buffer, unsigned long& bufSize, double left, double top, double &width, double &height, double zoom, bool onlyPixelData)
  {
    return getImageBufferEx(image, buffer, bufSize, left, top, width, height, zoom, onlyPixelData);
  }

  void disposeStr(char * &str)
  {
      delete[] str;
  }

  void getDicomTags(void * imagen, char * &output)
  {
      getTags(imagen, output);
  }

  void getDicomDimensions(void * image, int &width, int &height)
  {
    getDimensions(image, width, height);
  }

  void applyDicomWindowLevel(void * image, double center, double width)
  {
    applyWindowLevel(image, center, width);
  }

  void getDicomWindowLevel(void * imagen, double &center, double &width)
  {
    getWindowLevel(imagen, center, width);
  }

  void releaseDicomImage(void * imagen)
  {
      delete (DicomImageHandler *)imagen;
      imagen = NULL;
  }

  void getDicomDensity(void * imagen, int x, int y, int &density)
  {
      getDensity(imagen, x, y, density);
  }

  void getDicomRectRoi(void * imagen, int x, int y, int w, int h, double &median, double &stddev)
  {
    getRectRoi(imagen, x, y, w, h, median, stddev);
  }


  int getDicomImageDepth(void * imagen)
  {
      return getDepth(imagen);
  }

  int getDicomSamplesPerPixel(void * imagen)
  {
      return getSamplesPerPixel(imagen);
  }

  double getDicomZoom(void * imagen)
  {
      return getZoom(imagen);
  }

  void setDicomZoom(void * imagen, double zoom)
  {
      setZoom(imagen, zoom);
  }

  EP_Polarity getDicomPolarity(void * imagen)
  {
      return getPolarity(imagen);
  }

  void setDicomAngle(void * imagen, int angle)
  {
    rotate(imagen, angle);
  }

  int getDicomAngle(void * imagen)
  {
    DicomImageHandler * imgHandler = (DicomImageHandler *)imagen;
    return imgHandler->getAngle();
  }

  void HFlip(void * imagen)
  {
    DicomImageHandler * imgHandler = (DicomImageHandler *)imagen;
    imgHandler->getDicomImage()->flipImage(1,0);
  }

  void VFlip(void * imagen)
  {
    DicomImageHandler * imgHandler = (DicomImageHandler *)imagen;
    imgHandler->getDicomImage()->flipImage(0,1);
  }

  void setDicomPolarity(void * imagen, EP_Polarity polarity)
  {
      setPolarity(imagen, polarity);
  }

  int getFrameCount(void * imagen)
  {
    return getDicomFrameCount(imagen);
  }

  void setCurrentFrame(void * imagen, int frame)
  {
    setDicomCurrentFrame(imagen, frame);
  }

  void initDicomCFind(void * &cfind)
  {
      cfind = new DcmFindSCU();
      dcmtk::log4cplus::SharedAppenderPtr nullapp(new dcmtk::log4cplus::NullAppender());
      dcmtk::log4cplus::Logger log = dcmtk::log4cplus::Logger::getRoot();
      log.removeAllAppenders();
      log.addAppender(nullapp);
  }

  void DicomSaveToFile(void * imagen, char * file)
  {
    saveToFile(imagen, file);
  }

  void ReadDicomDir(char * file, DICOMDIROnStudy onStudy, DICOMDIROnPatient onPatient, DICOMDIROnSerie onSerie, DICOMDIROnImage onImage, void * ACaller)
  {
    DcmDicomDir dicomdir(file); 
    DcmDirectoryRecord *root = &(dicomdir.getRootRecord()); 
    DcmDirectoryRecord *PatientRecord =NULL; 
    DcmDirectoryRecord *StudyRecord = NULL; 
    DcmDirectoryRecord *SeriesRecord = NULL; 
    DcmDirectoryRecord *FileRecord = NULL;
    OFString patientId;
    OFString modality;
    OFString tmpString;
    OFString studyInstanceUID;
    OFString accessionNumber;
    OFString studyDate;	
    OFString lSeriesNumber;
    OFString lInstanceNumber;

    if(root != NULL) 
    { 
      while (((PatientRecord = root->nextSub(PatientRecord)) != NULL)) 
      { 
         if (PatientRecord->findAndGetOFString(DCM_PatientName, tmpString).good()) 
         { 
		    PatientRecord->findAndGetOFString(DCM_PatientID, patientId);
            onPatient(tmpString.c_str(), patientId.c_str(), ACaller);
         } 

         while (((StudyRecord = PatientRecord->nextSub(StudyRecord)) != NULL)) 
         { 
            StudyRecord->findAndGetOFString(DCM_StudyDescription, tmpString);
            StudyRecord->findAndGetOFString(DCM_StudyInstanceUID, studyInstanceUID);			
            StudyRecord->findAndGetOFString(DCM_AccessionNumber, accessionNumber);			
            StudyRecord->findAndGetOFString(DCM_StudyDate, studyDate);			
            onStudy(tmpString.c_str(), studyInstanceUID.c_str(), accessionNumber.c_str(), studyDate.c_str(), ACaller);

            while (((SeriesRecord = StudyRecord->nextSub(SeriesRecord)) != NULL)) 
            { 
              SeriesRecord->findAndGetOFString(DCM_SeriesNumber, lSeriesNumber);
              SeriesRecord->findAndGetOFString(DCM_Modality, modality);			  

              if (SeriesRecord->findAndGetOFString(DCM_SeriesDescription, tmpString).good()) 
              { 
                onSerie(tmpString.c_str(), lSeriesNumber.c_str(), modality.c_str(), ACaller);
              }
              else
              {   
                onSerie("No series description found.", lSeriesNumber.c_str(), modality.c_str(), ACaller);
              }
              while (((FileRecord = SeriesRecord->nextSub(FileRecord)) != NULL)) 
              { 
                FileRecord->findAndGetOFStringArray(DCM_InstanceNumber, lInstanceNumber);
                if (FileRecord->findAndGetOFStringArray(DCM_ReferencedFileID, tmpString).good()) 
                { 
                  onImage(tmpString.c_str(), lInstanceNumber.c_str(), ACaller);
                } 
             } 
            } 
         } 
       } 
     }

  }

  int DicomCFindPerformQuery(
    void * caller,
    void * cfind,
    CFindCallBack cfCallBack,
    const char *peer,
    unsigned int port,
    const char *ourAETitle,
    const char *peerAETitle,
    const char *abstractSyntax,
    const char *overrideKeys)
  {
    DcmFindSCU * lFind = (DcmFindSCU *)cfind;
    OFCondition cond = lFind->initializeNetwork(30);
    if(cond.bad())
    {
      return -1;
    }
    else
    {
      /* copy overridekeys to lOverrideKeys */
      OFList<OFString> lOverrideKeys;
      OFList<OFString> lFileNameList;

      std::string s1(overrideKeys);
      std::istringstream iss(s1);
      std::string s2;

      while( std::getline( iss, s2, ' ' ) )
      {
        OFString word(s2.c_str());
        lOverrideKeys.push_back(word);
      }

      if(strcmp(abstractSyntax, "P") == 0)
        abstractSyntax = UID_FINDPatientRootQueryRetrieveInformationModel;
      else
      if(strcmp(abstractSyntax, "S") == 0)
        abstractSyntax = UID_FINDStudyRootQueryRetrieveInformationModel;
      else
      if(strcmp(abstractSyntax, "O") == 0)
        abstractSyntax = UID_RETIRED_FINDPatientStudyOnlyQueryRetrieveInformationModel;
      else
        abstractSyntax = UID_FINDModalityWorklistInformationModel;

      DcmFindSCUmyCallback myCallBack(caller);
      myCallBack.setCallBack(cfCallBack);
      cond = lFind->performQuery(
        peer, port,
        ourAETitle, peerAETitle,
        abstractSyntax,
        EXS_Unknown,
        DIMSE_BLOCKING,
        0,
        ASC_DEFAULTMAXPDU,
        OFFalse,
        OFFalse,
        1,
        OFFalse,
        -1,
        &lOverrideKeys,
        &myCallBack
        );

      OFString temp_str;
      if (cond.bad())
      {
        DimseCondition::dump(temp_str, cond);
      }

      // destroy network structure
      cond = lFind->dropNetwork();
      if (cond.bad())
      {
       DimseCondition::dump(temp_str, cond);
      }

      return 0;
    }
  }


  void deleteDicomCFind(void * cfind)
  {
      delete (DcmFindSCU *)cfind;
      cfind = NULL;
  }


  void deleteBuffer(void * buffer)
  {
    delete[] (Uint8 *) buffer;
    buffer = NULL;
  }


}
