*"* use this source file for any type of declarations (class
*"* definitions, interfaces or type declarations) you need for
*"* components in the private section

** Featureextraction >>>
*TYPES: ltty_feature_vector TYPE STANDARD TABLE OF string WITH NON-UNIQUE KEY table_line.
**
*TYPES: BEGIN OF lsty_feature_vector,
*         feature_vector TYPE ltty_feature_vector,
*         name           TYPE string,
*       END OF lsty_feature_vector.
*TYPES: ltty_feature_vector_list TYPE STANDARD TABLE OF lsty_feature_vector WITH NON-UNIQUE KEY name.
**
*TYPES: BEGIN OF lsty_featureextraction,
*         _id           TYPE string,
*         predictions   TYPE ltty_feature_vector_list,
*         processedtime TYPE string,
*         status        TYPE string,
*       END OF lsty_featureextraction.
** Featureextraction >>>
**
** SimilarityScoring >>>
*TYPES: BEGIN OF lsty_vector,
*         id    TYPE string,
*         score TYPE string,
*       END OF lsty_vector.
*TYPES: ltty_vectors TYPE STANDARD TABLE OF lsty_vector WITH NON-UNIQUE KEY table_line.
**
*TYPES: BEGIN OF lsty_similarvector,
*         id             TYPE string,
*         similarvectors TYPE ltty_vectors,
*       END OF lsty_similarvector.
*TYPES: ltty_similarvectors TYPE STANDARD TABLE OF lsty_similarvector WITH NON-UNIQUE KEY id.
**
*TYPES: BEGIN OF lsty_similarityscoring,
*         _id           TYPE string,
*         predictions   TYPE ltty_similarvectors,
*         processedtime TYPE string,
*         status        TYPE string,
*       END OF lsty_similarityscoring.
** SimilarityScoring >>>
