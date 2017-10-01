#delimit ;

   infix
      year     1 - 20
      id_      21 - 40
      age      41 - 60
      degree   61 - 80
      region   81 - 100
      polviews 101 - 120
      class_   121 - 140
      xmarsex  141 - 160
      evstray  161 - 180
using GSS.dat;

label variable year     "Gss year for this respondent                       ";
label variable id_      "Respondent id number";
label variable age      "Age of respondent";
label variable degree   "Rs highest degree";
label variable region   "Region of interview";
label variable polviews "Think of self as liberal or conservative";
label variable class_   "Subjective class identification";
label variable xmarsex  "Sex with person other than spouse";
label variable evstray  "Have sex other than spouse while married";


label define gsp001x
   99       "No answer"
   98       "Don't know"
   89       "89 or older"
;
label define gsp002x
   9        "No answer"
   8        "Don't know"
   7        "Not applicable"
   4        "Graduate"
   3        "Bachelor"
   2        "Junior college"
   1        "High school"
   0        "Lt high school"
;
label define gsp003x
   9        "Pacific"
   8        "Mountain"
   7        "W. sou. central"
   6        "E. sou. central"
   5        "South atlantic"
   4        "W. nor. central"
   3        "E. nor. central"
   2        "Middle atlantic"
   1        "New england"
   0        "Not assigned"
;
label define gsp004x
   9        "No answer"
   8        "Don't know"
   7        "Extrmly conservative"
   6        "Conservative"
   5        "Slghtly conservative"
   4        "Moderate"
   3        "Slightly liberal"
   2        "Liberal"
   1        "Extremely liberal"
   0        "Not applicable"
;
label define gsp005x
   9        "No answer"
   8        "Don't know"
   5        "No class"
   4        "Upper class"
   3        "Middle class"
   2        "Working class"
   1        "Lower class"
   0        "Not applicable"
;
label define gsp006x
   9        "No answer"
   8        "Don't know"
   5        "Other"
   4        "Not wrong at all"
   3        "Sometimes wrong"
   2        "Almst always wrg"
   1        "Always wrong"
   0        "Not applicable"
;
label define gsp007x
   9        "No answer"
   8        "Don't know"
   3        "Never married"
   2        "No"
   1        "Yes"
   0        "Not applicable"
;


label values age      gsp001x;
label values degree   gsp002x;
label values region   gsp003x;
label values polviews gsp004x;
label values class_   gsp005x;
label values xmarsex  gsp006x;
label values evstray  gsp007x;


