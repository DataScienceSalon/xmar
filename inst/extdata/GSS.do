#delimit ;

   infix
      year     1 - 20
      id_      21 - 40
      age      41 - 60
      region   61 - 80
      polviews 81 - 100
      class_   101 - 120
      xmarsex  121 - 140
      evstray  141 - 160
using GSS.dat;

label variable year     "Gss year for this respondent                       ";
label variable id_      "Respondent id number";
label variable age      "Age of respondent";
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
label define gsp003x
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
label define gsp004x
   9        "No answer"
   8        "Don't know"
   5        "No class"
   4        "Upper class"
   3        "Middle class"
   2        "Working class"
   1        "Lower class"
   0        "Not applicable"
;
label define gsp005x
   9        "No answer"
   8        "Don't know"
   5        "Other"
   4        "Not wrong at all"
   3        "Sometimes wrong"
   2        "Almst always wrg"
   1        "Always wrong"
   0        "Not applicable"
;
label define gsp006x
   9        "No answer"
   8        "Don't know"
   3        "Never married"
   2        "No"
   1        "Yes"
   0        "Not applicable"
;


label values age      gsp001x;
label values region   gsp002x;
label values polviews gsp003x;
label values class_   gsp004x;
label values xmarsex  gsp005x;
label values evstray  gsp006x;


