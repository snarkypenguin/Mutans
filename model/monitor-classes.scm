; -*- mode: scheme; -*-
;-  Identification and Changes

;--
;	monitor-classes.scm -- Written by Randall Gray 
;	Initial coding: 
;		Date: 2016.08.01
;		Location: zero:/home/randall/Thesis/Example-Model/model/monitor-classes.scm
;
;	History:
"
    Copyright 2017 Randall Gray

    This file is part of Remodel.

    Remodel is free software: you can redistribute it and/or modify
    it under the terms of the GNU Affero General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    Remodel is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with Remodel.  If not, see <http://www.gnu.org/licenses/>.
"

;-  Code 
(include "remodel")

;; Monitors are derived from introspection since they really need to
;; be able to assess things in a temporally consistent way. The
;; timesteps used for agent-monitors would typically be shorter than
;; the timesteps for niche-level or domain-level monitors

(define-class <monitor>
	(inherits-from <introspection>)

	(state-variables file ;; for recording actions
						  assessment-domain ;; a list which defines the ambit of the monitor
						  domain*-options ;; set of sets of trees associated with domains
						  niche*-options  ;; set of sets of trees associated with niches
						  assessment-function ;; compares set of states
						  history  ;; records transitions
						  ;;introspection-targets ;; target agents 
						  specific-agents ;; agents which are always included
						  KnownGood KnownBad

						  ;; the monitor only selects things in the 
						  ;; specific-agents list or those for which
						  ;;    (selector self) 
						  ;; returns #f to indicate no action to be taken,
						  ;; #t to indicate that change is in the offing,
						  ;; a class to migrate to, or some symbolic value
						  ;; "history" is a state variable which 
						  ;; records the historical state of the monitor.
	 )
	)

"<monitor> agents play a special role, keeping an eye on subsets of
the population of agents.  Monitors may effect changes in the
composition of both the population and the constituents which comprise
an agent. In many ways, monitors are similar to introspection agents."


(define-class <agent-monitor> ;; test individual agents.  This is the one that will trigger the transition from  ABM to EBM at critical levels
  (inherits-from <monitor>)
  (state-variables assessment-function aggregate-data)
  ;; the aggregate data is a list of the individual assessments (as called in do-assessment)
  ;; which is used by pass-resolution, the analogue of emit-data.
)


(define-class <niche-monitor> ;; this is most likely to be associated with the 'provides and 'needs/requires lists
  (inherits-from <monitor>)
  (state-variables representation-alternatives
						 domain ;; contains candidate agents
						 niche-selector ;; selects the agents within the niche
						 )
  )

;; A domain monitor may have a number of niche level monitors which
;; are associated with it; they would typically be included in its
;; domain-list

(define-class <domain-monitor>  ;; most likely to be associated with the population of a landscape, but not necessarily so
  (inherits-from <monitor>)
  (state-variables domain-list domain-selector)
  ;; the domain list is the list of agents which comprise the notional domain
  ;; the monitor patrols
  )






;-  The End 


;;; Local Variables: 
;;; comment-end: "-;" ;;;
;;; comment-start: ";;; " ;;;
;;; mode: scheme ;;;
;;; outline-regexp: ";-+" ;;;
;;; comment-column: 0 ;;;
;;; End:
