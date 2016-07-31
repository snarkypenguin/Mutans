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

(define landscape-notes "
There are a number of classes defined under the 'landscape' umbrella, namely <ecoservice>, <patch>, 
<dynamic-patch>, the eponymous <landscape> and <habitat>.  They fill incrementally more complex roles:

   <ecoservice>     represents some fundamental (usually) biogenic aspect of the environment, though
                    though an <ecoservice> could be pressed into service to represent piles of sand, 
                    gravel, cement or compost at a gardening supply depot.

   <boundary>       provides an underlying shape for patches in the way that landscape provides a terrain
                    for habitats

   <patch>          maintains a list of <ecoservices>, a location, and a radius.  In principle, it
                    would be simple to refine <patch> to use arbitrary shapes (polygons or functionally 
                    bounded regions, for example)

   <dynamic-patch>  is a <patch> with a set of populations which are endemic to the patch. 
  
   <landscape>	     is an <environment> that provides a terrain in the form of a function

   <habitat>	     is derived from <landscape> and it manages a collection of dynamic patches.

   <grid-environment>


;-  The End 


;;; Local Variables: 
;;; comment-end: "-;" -;
;;; comment-start: ";;; " -;
;;; mode: scheme -;
;;; outline-regexp: ";-+" -;
;;; comment-column: 0 -;
;;; End:
