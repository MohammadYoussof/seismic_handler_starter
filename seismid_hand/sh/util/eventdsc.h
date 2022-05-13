
/* file eventdsc.h
 *      ==========
 *
 * version 45, 19-Jan-2007
 *
 * event descriptor definitions
 * K. Stammler, 25-Jul-93
 */


/*
 *
 *  SeismicHandler, seismic analysis software
 *  Copyright (C) 1996,  Klaus Stammler, Federal Institute for Geosciences
 *                                       and Natural Resources (BGR), Germany
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
 *
 */



/* error codes */
#define EveNOERROR        0
#define EveOFFSET         6100
#define EveCOMMENT_PTR    (EveOFFSET+1)    /* comment pointer inconsistency */
#define EveMEM_ALLOC      (EveOFFSET+2)    /* memory allocation error */
#define EveSTROVFL        (EveOFFSET+3)    /* string overflow */
#define EveILL_VALUE      (EveOFFSET+4)    /* illegal value */
#define EveOPEN_WRITE     (EveOFFSET+5)    /* error opening output file */
#define EveOPEN_READ      (EveOFFSET+6)    /* error opening input file */
#define EveMISSING_TERM   (EveOFFSET+7)    /* missing terminator */
#define EveUNKNOWN_LINE   (EveOFFSET+8)    /* unknown info item */
#define EveREAD_FLOAT     (EveOFFSET+9)    /* error reading float */
#define EveREAD_INT       (EveOFFSET+10)   /* error reading integer */
#define EveINFO_TWICE     (EveOFFSET+11)   /* info specified twice */
#define EveNO_MORE_EVENTS (EveOFFSET+12)   /* no more events */
#define EveFORMAT_ERR     (EveOFFSET+13)   /* format error in file */


/* constants */
#define EvTIMELTH 30
	/* maximum length of time string */
#define EvPHASELTH 20
	/* maximum length of phase names */
#define EvREGIONLTH 80
	/* maximum length of region name */
#define EvSTATIONLTH 10
	/* maximum length of station name */
#define EvFILTERLTH 80
	/* length of name of applied filter */
#define EvANALYSTLTH 10
	/* maximum length of analyst's abbreviation */
#define EvSOURCELTH 10
	/* maximum length of information source */
#define EvDESCR_LENGTH 25
	/* item description length */
#define EvFLAGLTH 6
	/* length of phase flag string */
#define EvUSERMAGNLTH 40
	/* length of user magnitude definition text */
#define EvVELMODLTH 40
	/* length of velocity model name */
#define EvLOCADDPARLTH 80
	/* number of floating parameters passed to external location programs */
#define EvMOMTENLTH 80
	/* values of moment tensor elements (string) */
#define EvMOMTENDESCRLTH 80
	/* description of moment tensor elements */
#define EvFPS_ANGLES 80
	/* values of fault plane angles (string) */
#define EvFPS_DESCRLTH 80
	/* description of FPS values */


/* empty entry specifiers */
#define EvEOS '\0'
	/* end of string */
#define EvEMPTY_ONSET_COUNT -1
	/* no onset count = secondary onset */
#define EvEMPTY_COMPONENT ' '
	/* undefined component */
#define EvEMPTY_PERIOD 0.0
	/* no period specified */
#define EvEMPTY_AMPLITUDE 0.0
	/* no amplitude specified */
#define EvEMPTY_SLOWNESS -1.0
	/* empty slowness */
#define EvEMPTY_AZIMUTH 1000.0
	/* empty azimuth */
#define EvEMPTY_DISTANCE 0.0
	/* empty distance */
#define EvEMPTY_QUALITY 0
	/* empty quality */
#define EvEMPTY_MAGNITUDE -100.0
	/* empty magnitude */
#define EvEMPTY_TEXT "not specified"
	/* empty text entries */
#define EvEMPTY_INPUT "."
	/* empty input */
#define EvEMPTY_LATITUDE -1000.0
	/* empty latitude */
#define EvEMPTY_LONGITUDE -1000.0
	/* empty longitude */
#define EvEMPTY_DEPTH -1000.0
	/* empty depth */
#define EvEMPTY_EVID 0
	/* empty event ID */
#define EvEMPTY_WEIGHT -1
	/* empty phase weight */
#define EvEMPTY_AMPLITUDE_TIME -1.0
	/* empty amplitude time */
#define EvEMPTY_STATIONS_USED 0
	/* empty number of stations used */
#define EvEMPTY_REGION_TABLE 0
	/* empty region table */
#define EvEMPTY_REGION_ID 0
	/* empty region ID */
#define EvEMPTY_ONSET_WDW -1.0
	/* empty onset accuracy */
#define EvEMPTY_ERR_FLOAT -1000.0
	/* empty error value of float values */
#define EvEMPTY_SIGNOISE 0.0
	/* empty signal/noise ratio */
#define EvEMPTY_RESIDUAL 0.0
	/* empty residual time */
#define EvEMPTY_RESID_CORR -9999.0
	/* empty frequencies */
#define EvEMPTY_FREQ -1000.0

/* event item names, an item name must not be a substring of another ! */
#define EvITEM_ARRAY          "Array name"
#define EvITEM_STATION        "Station code"
#define EvITEM_ONSET_TIME     "Onset time"
#define EvITEM_ONSET_TYPE     "Onset type"
#define EvITEM_ONSET_COUNT    "Following Onsets"
#define EvITEM_PHASE          "Phase name"
#define EvITEM_SIGN           "Sign"
#define EvITEM_COMPONENT      "Component"
#define EvITEM_PERIOD         "Period (sec)"
#define EvITEM_AMPLITUDE      "Amplitude (nm)"
#define EvITEM_AMPLITUDE_TIME "Amplitude Time (sec)"
#define EvITEM_AMPLITUDE_VEL  "Vel. Amplitude (nm/sec)"
#define EvITEM_LP_COMPONENT   "LP component"
#define EvITEM_LP_PERIOD      "LP period (sec)"
#define EvITEM_LP_AMPLITUDE   "LP amplitude"
#define EvITEM_BB_AMPLITUDE   "BB amplitude (nm/sec)"
#define EvITEM_BB_PERIOD      "BB period (sec)"
#define EvITEM_B_SLOWNESS     "Beam-Slowness (sec/deg)"
#define EvITEM_B_AZIMUTH      "Beam-Azimuth (deg)"
#define EvITEM_L_SLOWNESS     "Epi-Slowness (sec/deg)"
#define EvITEM_L_AZIMUTH      "Epi-Azimuth (deg)"
#define EvITEM_THEO_AZIM      "Theo. Azimuth (deg)"
#define EvITEM_THEO_BACK_AZIM "Theo. Backazimuth (deg)"
#define EvITEM_DISTANCE_DEG   "Distance (deg)"
#define EvITEM_DISTANCE_KM    "Distance (km)"
#define EvITEM_QUALITY        "Quality number"
#define EvITEM_MS             "Magnitude ms"
#define EvITEM_MB             "Magnitude mb"
#define EvITEM_ML             "Magnitude ml"
#define EvITEM_MW             "Magnitude mw"
#define EvITEM_MBB            "Broadband Magnitude"
#define EvITEM_MU             "User Magnitude"
#define EvITEM_MEAN_MS        "Mean Magnitude ms"
#define EvITEM_MEAN_MB        "Mean Magnitude mb"
#define EvITEM_MEAN_ML        "Mean Magnitude ml"
#define EvITEM_MEAN_MW        "Mean Magnitude mw"
#define EvITEM_MEAN_MBB       "Mean BB Magnitude"
#define EvITEM_MEAN_MU        "Mean User Magnitude"
#define EvITEM_MU_DESCR       "User Magn. Description"
#define EvITEM_REGION         "Source region"
#define EvITEM_COMMENT        "Comment"
#define EvITEM_LATITUDE       "Latitude"
#define EvITEM_LONGITUDE      "Longitude"
#define EvITEM_LOC_METHOD     "Location method"
#define EvITEM_LOC_QUALITY    "Location quality"
#define EvITEM_LOC_VELMOD     "Velocity Model"
#define EvITEM_LOC_ADDPAR     "Location Input Params"
#define EvITEM_FILTER         "Applied filter"
#define EvITEM_DEPTH          "Depth (km)"
#define EvITEM_DEPTH_TYPE     "Depth type"
#define EvITEM_ORIGIN_TIME    "Origin time"
#define EvITEM_EVID           "Event ID"
#define EvITEM_WEIGHT         "Weight"
#define EvITEM_ONSET_ACC      "Onset Accuracy"
#define EvITEM_REF_LATITUDE   "Reference Latitude"
#define EvITEM_REF_LONGITUDE  "Reference Longitude"
#define EvITEM_REF_NAME       "Reference Location Name"
#define EvITEM_ANALYST        "Analyst"
#define EvITEM_STATIONS_USED  "No. of Stations used"
#define EvITEM_REGION_TABLE   "Region Table"
#define EvITEM_REGION_ID      "Region ID"
#define EvITEM_EVENT_TYPE     "Event Type"
#define EvITEM_SOURCE         "Source of Information"
#define EvITEM_AP_SOURCE      "Ampl&Period Source"
#define EvITEM_ONSET_WDW_L    "Onset Window Left"
#define EvITEM_ONSET_WDW_R    "Onset Window Right"
#define EvITEM_ERR_LAT        "Error in Latitude (km)"
#define EvITEM_ERR_LON        "Error in Longitude (km)"
#define EvITEM_ERR_DEPTH      "Error in Depth (km)"
#define EvITEM_ERR_ORIGIN     "Error in Origin Time"
#define EvITEM_ERR_SMAJOR     "Error Ellipse Major"
#define EvITEM_ERR_SMINOR     "Error Ellipse Minor"
#define EvITEM_ERR_MAJSTRIKE  "Error Ellipse Strike"
#define EvITEM_AZIM_MAX_GAP   "Max Azimuthal Gap (deg)"
#define EvITEM_RESID_RMS      "RMS of Residuals (sec)"
#define EvITEM_PHASE_FLAGS    "Phase Flags"
#define EvITEM_SIGNOISE       "Signal/Noise"
#define EvITEM_RESIDUAL       "Residual Time"
#define EvITEM_RESID_CORR     "Residual Correction"
#define EvITEM_PICK_TYPE      "Pick Type"
#define EvITEM_CORNERFREQ     "Corner Frequency"
#define EvITEM_LOWFREQLEVEL   "Low Frequency Level"
#define EvITEM_MOMTEN         "Moment Tensor Elements"
#define EvITEM_MOMTEN_DESCR   "Moment Tensor Descr."
#define EvITEM_M0             "Scalar Moment"
#define EvITEM_FPS_ANGLES     "Fault Plane Solution"
#define EvITEM_FPS_DESCR      "FPS Description"
#define EvITEM_END_OF_EVENT   "--- End of Phase ---"


/* value description texts */
#define EvTEXT_ONSET_EMERGENT         "emergent"
#define EvTEXT_ONSET_IMPULSIVE        "impulsive"
#define EvTEXT_SIGN_POSITIVE          "positive"
#define EvTEXT_SIGN_NEGATIVE          "negative"
#define EvTEXT_LMETHOD_CORRBEAM       "array beam (calibrated)"
#define EvTEXT_LMETHOD_UNCORRBEAM     "array beam (uncorrected)"
#define EvTEXT_LMETHOD_RESIDCORR      "array beam (resid.corr.)"
#define EvTEXT_LMETHOD_HYPO           "hypoellipse"
#define EvTEXT_LMETHOD_LOCSAT         "locsat"
#define EvTEXT_LMETHOD_HYPOCENTER     "hypocenter"
#define EvTEXT_LMETHOD_HYPO71         "hypo71"
#define EvTEXT_LMETHOD_HYPOSAT        "hyposat"
#define EvTEXT_LMETHOD_RELTRAV        "relative travel times"
#define EvTEXT_LMETHOD_EXTERNAL       "external"
#define EvTEXT_ONSET_ACC_SEC          "second"
#define EvTEXT_ONSET_ACC_MSEC         "millisecond"
#define EvTEXT_EVTYPE_TELE_QUAKE      "teleseismic quake"
#define EvTEXT_EVTYPE_NUCLEAR         "nuclear explosion"
#define EvTEXT_EVTYPE_REGIO_QUAKE     "regional quake"
#define EvTEXT_EVTYPE_LOCAL_QUAKE     "local quake"
#define EvTEXT_EVTYPE_BLAST           "quarry blast"
#define EvTEXT_EVTYPE_MINING          "mining event"
#define EvTEXT_REGTABLE_FLINN_ENGDAHL "Flinn-Engdahl"
#define EvTEXT_REGTABLE_GERMAN_GEO    "GEO_REG"
#define EvTEXT_REGTABLE_SEISMOTEC     "SEIS_REG"
#define EvTEXT_APSRC_DIRECT           "direct"
#define EvTEXT_APSRC_BEAM             "beam trace"
#define EvTEXT_APSRC_ALIGN            "align trace"
#define EvTEXT_DEPTH_PRESET           "(n) preset"
#define EvTEXT_DEPTH_ESTIMATED        "(g) estimated"
#define EvTEXT_DEPTH_FREE             "( ) free"
#define EvTEXT_DEPTH_POOR             "(?) poorly constrained"
#define EvTEXT_DEPTH_LESSWELL         "(*) less well constrained"
#define EvTEXT_DEPTH_RELIABLE         "(d) reliable"
#define EvTEXT_DEPTH_EXTERNAL         "external"
#define EvTEXT_LOCQ_TOOWEAK           "too weak"
#define EvTEXT_LOCQ_INCOHERENT        "incoherent"
#define EvTEXT_LOCQ_NOBEARING         "no bearing"
#define EvTEXT_LOCQ_REGION            "region"
#define EvTEXT_LOCQ_RELIABLE          "reliable"
#define EvTEXT_PICKTYPE_MANUAL        "manual"
#define EvTEXT_PICKTYPE_AUTO          "automatic"
#define EvTEXT_PICKTYPE_THEO          "theoretical"


/* output formats */
#define EvFORMAT_ONSET_COUNT "%d"
#define EvFORMAT_PERIOD "%5.2f"
#define EvFORMAT_AMPLITUDE "%4.1f"
#define EvFORMAT_SLOWNESS "%5.2f"
#define EvFORMAT_AZIMUTH "%7.2f"
#define EvFORMAT_DISTANCE "%6.3f"
#define EvFORMAT_QUALITY "%d"
#define EvFORMAT_MAGNITUDE "%3.1f"
#define EvFORMAT_LATITUDE "%+7.3f"
#define EvFORMAT_LONGITUDE "%+8.3f"
#define EvFORMAT_DEPTH "%5.1f"
#define EvFORMAT_EVID "%09ld"
#define EvFORMAT_WEIGHT "%d"
#define EvFORMAT_AMPLITUDE_TIME "%5.2f"
#define EvFORMAT_STATIONS_USED "%d"
#define EvFORMAT_REGION_ID "%d"
#define EvFORMAT_ONSET_WDW "%6.3f"
#define EvFORMAT_ERR_FLOAT "%6.2f"
#define EvFORMAT_SIGNOISE "%5.1f"
#define EvFORMAT_RESIDUAL "%6.2f"
#define EvFORMAT_FREQ "%6.1e"
#define EvFORMAT_MOMENT "%6.2e"


/* value bounds */
#define EvLIMIT_LO_ONSET_COUNT 0
#define EvLIMIT_HI_ONSET_COUNT 20
#define EvLIMIT_LO_PERIOD 0.1
#define EvLIMIT_HI_PERIOD 1000.0
#define EvLIMIT_LO_AMPLITUDE 0.01
#define EvLIMIT_HI_AMPLITUDE 10000.0
#define EvLIMIT_LO_SLOWNESS 0.1
#define EvLIMIT_HI_SLOWNESS 30.0
#define EvLIMIT_LO_AZIMUTH 0.0
#define EvLIMIT_HI_AZIMUTH 360.0
#define EvLIMIT_LO_DISTANCE_DEG 0.1
#define EvLIMIT_HI_DISTANCE_DEG 180.0
#define EvLIMIT_LO_DISTANCE_KM 0.1
#define EvLIMIT_HI_DISTANCE_KM 40000.0
#define EvLIMIT_LO_QUALITY 1
#define EvLIMIT_HI_QUALITY 1000
#define EvLIMIT_LO_MAGNITUDE -4.0
#define EvLIMIT_HI_MAGNITUDE 11.0
#define EvLIMIT_LO_LATITUDE -90.0
#define EvLIMIT_HI_LATITUDE 90.0
#define EvLIMIT_LO_LONGITUDE 0.0
#define EvLIMIT_HI_LONGITUDE 180.0
#define EvLIMIT_LO_DEPTH -9.0
#define EvLIMIT_HI_DEPTH 800.0
#define EvLIMIT_LO_EVID 0
#define EvLIMIT_HI_EVID 999999999L
#define EvLIMIT_LO_WEIGHT 0
#define EvLIMIT_HI_WEIGHT 4
#define EvLIMIT_LO_AMPLITUDE_TIME 0.0
#define EvLIMIT_HI_AMPLITUDE_TIME 1000.0
#define EvLIMIT_LO_STATIONS_USED 1
#define EvLIMIT_HI_STATIONS_USED 1000
#define EvLIMIT_LO_REGION_ID 0
#define EvLIMIT_HI_REGION_ID 10000
#define EvLIMIT_LO_ONSET_WDW 0.0
#define EvLIMIT_HI_ONSET_WDW 1000.0

/* phase flag characters */
#define EvFLAG_MAGNITUDE  'M'
#define EvFLAG_MAGN_INDIC '<'
#define EvFLAG_LOCATION   'L'
#define EvFLAG_BEAM       'B'
#define EvFLAG_CALIBEVENT 'C'
#define EvFLAG_IGNORE     'I'
#define EvFLAG_TELEX      'T'
#define EvFLAG_ONSETCORR  'O'



/* types */

/* return status type */
typedef int EvStatusT;

/* floating point numbers */
typedef float EvFloatT;

/* quality type */
typedef int EvQualityT;

/* magnitude type */
typedef float EvMagnT;

/* onset type */
typedef enum {
	EvcOnsetUndefined,      /* undefined onset */
	EvcOnsetEmergent,       /* emergent onset */
	EvcOnsetImpulsive       /* impulsive onset */
} EvOnsetT;

/* onset sign */
typedef enum {
	EvcSignNegative = -1,   /* negative sign */
	EvcSignUndefined = 0,   /* sign not known */
	EvcSignPositive = 1     /* positive sign */
} EvSignT;

/* depth type */
typedef enum {
	EvcDepthUndefined,      /* undefined */
	EvcDepthPreset,         /* (PDE: n) preset depth */
	EvcDepthEstimated,      /* (PDE: g) estimated depth */
	EvcDepthFree,           /* (PDE:  ) free depth */
	EvcDepthPoor,           /* (PDE: ?) poorly constrained */
	EvcDepthLessWell,       /* (PDE: *) less well constrained */
	EvcDepthReliable,       /* (PDE: d) reliable depth */
	EvcDepthExternal        /* external information source */
} EvDepthT;

/* location method */
typedef enum {
	EvcLocMethUndefined,    /* undefined */
	EvcLocMethCorrBeam,     /* plane wave approximation, corrected beam */
	EvcLocMethUncorrBeam,   /* plane wave approximation, uncorrected beam */
	EvcLocMethResidCorr,    /* plane wave approximation, residual corrected */
	EvcLocMethHypo,         /* Hypoellipse */
	EvcLocMethLocsat,       /* LocSAT */
	EvcLocMethHypocenter,   /* Hypocenter */
	EvcLocMethHypo71,       /* Hypo71 */
	EvcLocMethHyposat,      /* HypoSAT */
	EvcLocMethRelTrav,      /* relative travel times */
	EvcLocMethExternal      /* external information source */
} EvLocMethT;

/* location quality */
typedef enum {
	EvcLocQualUndefined,    /* undefined */
	EvcLocQualTooWeak,      /* too weak signal */
	EvcLocQualIncoherent,   /* incoherent signal */
	EvcLocQualNoBearing,    /* no bearing, only slowness */
	EvcLocQualRegion,       /* only region given */
	EvcLocQualReliable      /* reliable location */
} EvLocQualT;

/* onset accuracy */
typedef enum {
	EvcOnsetAccUndefined,   /* undefined */
	EvcOnsetAccSecond,      /* seconds */
	EvcOnsetAccMillisecond  /* milliseconds */
} EvOnsetAccT;

typedef struct {
	int          length;    /* allocation length in bytes */
	char         *text;     /* pointer to comment text */
} EvCommentT;

typedef enum {
	EvcEventTypeUndefined,  /* undefined */
	EvcEventTypeTeleQuake,  /* teleseimic earthquake */
	EvcEventTypeNuclear,    /* nuclear expolsion */
	EvcEventTypeRegioQuake, /* regional earthquake */
	EvcEventTypeLocalQuake, /* local earthquake */
	EvcEventTypeBlast,      /* quarry blast */
	EvcEventTypeMining      /* mining event */
} EvEventTypeT;

typedef enum {
	EvcApSourceUndefined,   /* undefined */
	EvcApSourceDirect,      /* from station trace directly */
	EvcApSourceBeam,        /* from beam */
	EvcApSourceAlign        /* from align trace */
} EvApsrcT;

typedef enum {
	EvcRegionTableUndefined,    /* undefined */
	EvcRegionTableFlinnEngdahl, /* Flinn-Endahl */
	EvcRegionTableGermanGeo,    /* german geographic regions */
	EvcRegionTableSeismoTec     /* seismotectonic regions */
} EvRegionTableT;

typedef enum {
	EvcPickTypeUndefined,       /* undefined */
	EvcPickTypeManual,          /* manually picked phase */
	EvcPickTypeAuto,            /* automatically picked phase */
	EvcPickTypeTheo             /* theoretical phase */
} EvPickTypeT;

typedef enum {
	EvcMagMl,                   /* magnitude ml */
	EvcMagMb,                   /* magnitude mb */
	EvcMagMs,                   /* magnitude MS */
	EvcMagMw,                   /* magnitude Mw */
	EvcMagMbb,                  /* broadband magnitude */
	EvcMagMu,                   /* user magnitude */
	EvcMagLast                  /* last entry */
} EvMagnKindT;

typedef struct {
	EvFloatT    lat_km;                   /* error in latitude (km) */
	EvFloatT    lon_km;                   /* error in longitude (km) */
	EvFloatT    dep;                      /* depth error (km) */
	EvFloatT    orig;                     /* error in origin time (sec) */
	EvFloatT    smajor;                   /* semi major axis */
	EvFloatT    sminor;                   /* semi minor axis */
	EvFloatT    majstrike;                /* major axis strike (deg) */
	EvFloatT    azim_max_gap;             /* maximum azimuthal gap (deg) */
	EvFloatT    resid_rms;                /* RMS of residuals (sec) */
} EvEpiErr;

/* event information block */
typedef struct {
	/* information related to phases */
	long        evid;                     /* event ID */
	char        phase[EvPHASELTH+1];      /* phase name */
	int         weight;                   /* phase weight */
	EvQualityT  quality;                  /* quality */
	char        filter[EvFILTERLTH+1];    /* applied filter */
	char        station[EvSTATIONLTH+1];  /* official station code */
	char        array[EvSTATIONLTH+1];    /* array name */
	char        onset_time[EvTIMELTH+1];  /* date & time of event */
	EvOnsetT    onset_type;               /* onset type */
	EvOnsetAccT onset_acc;                /* onset accuracy */
	float       onset_wdw_l;              /* left onset window (in sec) */
	float       onset_wdw_r;              /* right onset window (in sec) */
	int         onset_count;              /* onset count (no of foll. onsets) */
	EvSignT     sign;                     /* sign of onset */
	char        component;                /* component name (uppercase) */
	EvFloatT    period;                   /* period in sec */
	EvFloatT    amplitude;                /* amplitude */
	EvFloatT    amplitude_time;           /* time of ampl. measurement (sec) */
	EvFloatT    amplitude_vel;            /* velocity amplitude (nm/sec) */
	EvApsrcT    ap_source;                /* amplitude & period source */
	char        lp_component;             /* LP component */
	EvFloatT    lp_period;                /* LP period in sec */
	EvFloatT    lp_amplitude;             /* LP amplitude */
	EvFloatT    bb_period;                /* BB period (sec) */
	EvFloatT    bb_amplitude;             /* BB amplitude (nm/sec) */
	EvMagnT     mag[EvcMagLast];          /* ms magnitude */
	char        mu_descr[EvUSERMAGNLTH+1];/* definition of user magnitude */
	EvFloatT    signoise;                 /* signal/noise ratio */
	EvFloatT    residual;                 /* residual time */
	EvFloatT    resid_corr;               /* correction for residual */
	EvPickTypeT pick_type;                /* pick type (manual, automatic, ..) */
	EvFloatT    b_slowness;               /* beam slowness in sec/deg */
	EvFloatT    b_azimuth;                /* beam azimuth in deg */
	EvFloatT    l_slowness;               /* epicenter slowness in sec/deg */
	EvFloatT    l_azimuth;                /* epicenter azimuth in deg (back-az)*/
	/* information related to stations */
	EvFloatT    theo_azim;                /* theoretical azimuth */
	EvFloatT    theo_back_azim;           /* theoretical back azimuth */
	EvFloatT    distance_deg;             /* epicentral distance in deg */
	EvFloatT    distance_km;              /* epicentral distance in km */
	/* information related to events */
	EvRegionTableT region_table;          /* region table */
	int         region_id;                /* ID number of region */
	char        region[EvREGIONLTH+1];    /* name of region */
	EvCommentT  comment;                  /* comment */
	EvFloatT    latitude;                 /* epicentre latitude in degrees */
	EvFloatT    longitude;                /* epicentre longitude in degrees */
	EvFloatT    depth;                    /* source depth in km */
	EvDepthT    depth_type;               /* type of depth determination */
	char        origin_time[EvTIMELTH+1]; /* origin time */
	EvLocMethT  loc_method;               /* location method */
	EvLocQualT  loc_quality;              /* location quality */
	char        velmod[EvVELMODLTH+1];    /* name of velocity model used */
	char        loc_addpar[EvLOCADDPARLTH+1]; /* flags passed to external loc.progs*/
	EvFloatT    ref_latitude;             /* latitude of reference location */
	EvFloatT    ref_longitude;            /* longitude of reference location */
	char        ref_name[EvSTATIONLTH+1]; /* name of reference location */
	char        analyst[EvANALYSTLTH+1];  /* analyst's abbreviation */
	int         stations_used;            /* number of stations used */
	EvMagnT     meanmag[EvcMagLast];      /* mean magnitudes */
	EvEventTypeT event_type;              /* type of event */
	char        source[EvSOURCELTH+1];    /* source of information */
	char        phase_flags[EvFLAGLTH+1]; /* phase flags */
	char        momten[EvMOMTENLTH];      /* moment tensor elements */
	char        momten_descr[EvMOMTENDESCRLTH]; /* description of above values */
	float       m0;                       /* scalar moment */
	char        fps_angles[EvFPS_ANGLES]; /* fault plane angles */
	char        fps_descr[EvFPS_DESCRLTH]; /* description of fault plane values */
	float       cornerfreq;               /* corner frequency of spectrum */
	float       lowfreqlevel;             /* low frequency level of spectrum */
	EvEpiErr    err;                      /* errors of parameters */
} EvEventT;

/* input & output routines */
typedef void EvGetRoutineT( FILE *fp, EvEventT *event,
	BOOLEAN *eof, EvStatusT *status );
typedef void EvPutRoutineT( FILE *fp, EvEventT *event,
	EvStatusT *status );



/* prototypes */


/*-------------------------------------------------------------------*/


void EvInitializeEvent( EvEventT *event );

/* Initializes event structure
 *
 * parameters of routine
 * EvEventT   *event;       output; event structure to be initialized
 */


/*-------------------------------------------------------------------*/


void EvReadEventInteractive( FILE *inp, EvEventT *event,
	BOOLEAN *eof, EvStatusT *status );

/* Reads event from terminal
 *
 * parameters of routine
 * FILE       *inp;          input; input channel (usually stdin)
 * EvEventT   *event;        output; event read
 * BOOLEAN    *eof;          output; no more events to be entered
 * EvStatusT  *status;       output; return status
 */


/*-------------------------------------------------------------------*/


void EvGetEvent( FILE *file, EvEventT *event, BOOLEAN *eof,
	EvStatusT *status );

/* Reads one event block from file "file".  If "eof" is TRUE then
 * the end of file is found and "event" contains no valid information.
 * In this case "status" is set to an error code only if the file is
 * incorrectly terminated.
 *
 * parameters of routine
 * FILE       *file;       input; pointer to input file
 * EvEventT   *event;      output; event read from file
 * BOOLEAN    *eof;        output; end of file found
 * EvStatusT  *status;     output; return status
 */


/*-------------------------------------------------------------------*/


void EvReadLineInteractive( FILE *inp, int length, char text[] );

/* read text from standard input
 *
 * parameters of routine
 * FILE       *inp;         input; input channel (usually stdin)
 * int        length;       input; maximum length of output string
 * char       text[];       output; text read in
 */


/*-------------------------------------------------------------------*/


void EvPutEvent( FILE *file, EvEventT *event, EvStatusT *status );

/* Writes event descriptor to file
 *
 * parameters of routine
 * FILE       *file;         input; pointer to output file
 * EvEventT   *event;        input; event to write to file
 * EvStatusT  *status;       output; return status
 */


/*-------------------------------------------------------------------*/


void EvCreateEventfile( char filename[], EvEventT *event,
	EvStatusT *status );

/* Creates new event file named "filename".  An already existing
 * file of the same name is overwritten
 *
 * parameters of routine
 * char       filename[];      input; name of event file to be created
 * EvEventT   *event;          input; event information
 * EvStatusT  *status;         output; return status
 */


/*-------------------------------------------------------------------*/


void EvAppendEventfile( char filename[], EvEventT *event,
	EvStatusT *status );

/* Appends event to existing file named "filename".  If the file does
 * not yet exist it will be created.
 *
 * parameters of routine
 * char       filename[];      input; name of event file to be appended
 * EvEventT   *event;          input; event information
 * EvStatusT  *status;         output; return status
 */


/*-------------------------------------------------------------------*/


void EvReformatEventfile( char infile[], char outfile[],
	EvGetRoutineT *get_routine, EvPutRoutineT *put_routine,
	EvStatusT *status );

/* Reformats event file using specified event input and output
 * routines.
 *
 * parameters of routine
 * char           infile[];       input; name of input file
 * char           outfile[];      input; name of output file
 * EvGetRoutineT  *get_routine;   input; input routine
 * EvPutRoutineT  *put_routine;   input; output routine
 * EvStatusT      *status;        output; return status
 */


/*-------------------------------------------------------------------*/


void EvAddComment( EvCommentT *comment, char *new_text,
	EvStatusT *status );

/* Appends new text to comment of event
 *
 * parameters of routine
 * EvCommentT *comment;       modify; comment to be modified
 * char       new_text[];     input; text to be added
 * EvStatusT  *status,        output; return status
 */


/*-------------------------------------------------------------------*/


void EvDeleteComment( EvCommentT *comment, EvStatusT *status );

/* Deletes comment
 *
 * parameters of routine
 * EvCommentT *comment;       modify; comment to be deleted
 * EvStatusT  *status;        output; return status
 */


/*-------------------------------------------------------------------*/


void EvReadLineInteractive( FILE *inp, int length, char text[] );

/* read text from standard input
 *
 * parameters of routine
 * FILE       *inp;         input; input channel (usually stdin)
 * int        length;       input; maximum length of output string
 * char       text[];       output; text read in
 */


/*-------------------------------------------------------------------*/


void EvShowEmptyValues( BOOLEAN show );

/* Sets display flag for empty values to TRUE or FALSE
 *
 * parameters of routine
 * BOOLEAN    show;         input; display flag
 */


/*-------------------------------------------------------------------*/
