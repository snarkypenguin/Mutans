; -*- mode: scheme; -*-
;-  Identification and Changes

;--
;	landscape-notes.scm -- Written by Randall Gray 
;	Initial coding: 
;		Date: 2016.07.14
;		Location: zero:/home/randall/Thesis/Example-Model/model/landscape-notes.scm
;
;	History:
;

(define landscape-notes"
There are a number of classes defined under the 'landscape' umbrella, namely <ecoservice>, <patch>, 
<dynamic-patch>, the eponymous <landscape> and <habitat>.  They fill incrementally more complex roles:

   <ecoservice>     represents some fundamental (usually) biogenic aspect of the environment, though
                    though an <ecoservice> could be pressed into service to represent piles of sand, 
                    gravel, cement or compost at a gardening supply depot.

   <patch>          maintains a list of <ecoservices>, a location, and a radius.  In principle, it
                    would be simple to refine <patch> to use arbitrary shapes (polygons or functionally 
                    bounded regions, for example)

   <dynamic-patch>  is a <patch> with a set of populations which are endemic to the patch. 
  
;-  The End 


;;; Local Variables: 
;;; comment-end: "-;" -;
;;; comment-start: ";;; " -;
;;; mode: scheme -;
;;; outline-regexp: ";-+" -;
;;; comment-column: 0 -;
;;; End:
