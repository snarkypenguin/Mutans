<TeXmacs|1.0.7.2>

<style|article>

<\body>
  \ <doc-data|<doc-title| Some interesting
  title>|<doc-author-data|<author-name|Randall Gray and Simon Wotherspoon>>>

  <\abstract>
    This paper explores the notion that dynamically changing the
    representation of submodels within an ensemble of models may provide a
    means of optimising the overall utility of a model. \ DESCRIPTION. \ TOY
    MODEL. RESULTS.
  </abstract>

  <section|Introduction>

  Many models are constructed as an ensemble of submodels. \ Often these
  submodels are integrated almost seamlessly and the aggregate system
  presents itself, like an organism, as an indivisible entity. \ At the other
  end of the spectrum models may be comprised of quite distinct submodels
  which retain their independence and stand, in some sense, as models in
  their own right.\ 

  There are quite a number of well established ways of integrating submodels,
  many of which achieve efficiency, either in run-time or accuracy, by
  running different components of the model at different, possibly varying
  temporal or spatial scales. There may still be scope for improvement,
  however. \ 

  The scope of ecosystem models is steadily increasing (NWS, ATLANTIS,
  EcoSIM/EcoPath/EwE, ATLAS)<with|color|red| [REFS]>. Our models are
  including more functional groups within the ecosystem and the interactions
  between components are becoming more detailed. \ As our climate changes and
  the environment we live in and use drifts further from its familiar state,
  there is a corresponding need to manage our interactions with the
  ecosystems around us more carefully, and this often implies a greater
  reliance on detailed modelling of the system. \ This explosion of demand
  for detail is costly in terms of computational load -- modelling whole
  populations of some endangered species at an individual level may be very
  good at capturing the population's vulnerability to exceptional events, but
  it takes a long time. Worse still, much of the simulation time may be spent
  with the model in a largely unchallenging part of its state-space.\ 

  As a simple illustration we might consider the motion of a mass capable of
  acceleration, say a rocket or a car. The rules of Newtonian motion make a
  good model as long as our mass doesn't move too quickly, and while
  relativistic motion is accurate at low speeds, it is more expensive to
  calculate. A simulation of a spacecraft (or car) that approaches the speed
  of light would spend a lot of time calculating relativistic motion when a
  simpler model would be adequate. It is easy to imagine that we might use
  Newton's equations when the velocities are low (most of the state space),
  but shift to relativistic motion when we stray from the areas where Newton
  is comfortably accurate.

  So the notion is that we might change the <em|representation> of a submodel
  based on its location in its state-space, or indeed the state of the model
  as a whole. In some sense we do this in our simulation models anyway. \ In
  the most simple case, time-steps or spatial resolutions are changed, but
  beyond that clauses may be by-passed if the local state of the simulation
  meets certain criteria, and additional calculations might be performed to
  reduce the error when the state is changing rapidly.\ 

  The experience garnered in the several large scale MSE-style ecosystem
  modelling projects has shown that a more systematic approach is needed.
  \ The burden of maintaining all of a complex model's state when it is in a
  region where a simple state may perform better is unnecessary, and the
  potential for significant improvements in run-time and the reduction of
  error seem achievable. \ There are a number of aspects of the notion that
  need to be teased out and examined closely: how the state of a sub-model
  changes through transitions (mutations) in representation, how we decide
  <em|when> to change representation and how expensive that might be, and how
  to manage the error associated with the potential loss of state information
  are all important issues which need investigation. \ More fundamentally
  there needs to be, <em|prima facie>, a demonstration that the notion might
  be worth pursuing; this is the intent of this paper.

  This approach to the problem has been developed in the light of experience
  with several large scale human-ecosystem interaction models. \ In these
  studies work [PMEZ, NWS, and now Ningaloo], a number of submodels are run
  concurrently, many of which represent individuals or small aggregations of
  individuals. \ One of the significant components in each of these projects
  is simulating the interaction between simulated organisms and contaminant
  plumes. This particular facet of the models has very expensive in terms of
  run-time -- in the NWS model it increased the time taken by roughly an
  order of magnitude. \ In these PMEZ and NWS, a considerable amount of time
  was unprofitably spent by the submodels in areas of the state space of the
  model where no interaction with contaminant plumes was possible. \ Alas,
  heuristics can only go so far and so our search for alternative ways of
  reducing the computational load without undue loss.

  <subsection|Scope>

  The aim of this paper is not to develop a complete body of work supporting
  the use of models which mutate according to where they might be in their
  state-space, rather it is to establish that the idea is reasonable. This is
  a specific example which has been carefully constructed to maximise its
  chance of supporting the hypothesis, and though we might make general
  observations that lie outside this unassuming target, the pursuit of their
  implications lie outside our immediate intent.\ 

  We will consider a model of organisms moving along a simple migratory path
  which intersects a region with varying levels of contamination. \ The model
  isn't really intended to be an accurate representation of any real system;
  since its role is to help explore some of the issues associated with
  changing representations and to test the hypothesis that we might gain
  efficiency in simulation time, representational accuracy, or both, its
  proximity to any real situation must be viewed as happy coincidence. \ 

  Our toy uses rather obvious basic representations of individuals and
  populations as the ``natural variants'' and the mutating model simply
  oscillates between them. The population model makes the assumption that the
  contaminant contact with the population is distributed through the
  population according to the density function which describes the population
  and that this is adequate to characterise the uptake based on the
  likelihood of an individual being at any given location within the
  population's radius, while the individual-based simulation makes the
  assumption that the individual trajectories have an important impact on the
  contaminant loads of the individuals . Unlike the example of Newtonian and
  Relativistic motion, the state-spaces of the basic models we consider
  differ, though in only a small way. \ Individuals follow trajectories whose
  mean is (more or less) the migratory circle. \ Most of this position
  information is not pertinent for the population model -- it's natural
  representation is to simply position itself at the appropriate point on the
  migratory path for any given time. The transition from individuals to
  population and population to individuals involves the loss and
  reconstruction of this fine-scale position data. In this context, we assume
  that the location of any given individual would be thoroughly randomised
  over the interval of time usually spent in a population representation, and
  so we can reconstruct a plausible location based on the distribution of
  individuals within the population's domain.

  <section|Experimentation>\ 

  The comparison of the three modelling schemes is based on the run-times for
  each representation, the correspondence between the total contaminant loads
  across the biomass through time, a comparison of variability in contaminant
  load through time amongst the representations, and the sensitivity to
  variability in the nature of the plume.\ 

  Our strategy is to first establish the equivalence of the mutating model
  and the individual-based representation in terms of their results. \ This
  component of the study also provides us with data for comparing the
  relative efficiency of the representations in terms of run-time. We
  generate forty trials each simulating forty individuals for both the
  individual-based and mutating models. \ The population models are
  deterministic and one run is sufficient.

  Second, we incorporate the data from the mutating model in a matching set
  to double the sample size, and run a corresponding set of trials using an
  asymmetric plume. These runs are compared with each other and with
  corresponding symmetric and asymmetric runs of the population-based
  representation. Ideally, the results with an asymmetric plume would be
  consistent across the model representations, though the results will show
  this is not the case.

  <subsection|Basic model structure>

  There are a number of attributes of the system which are common to all of
  its representations. Each of the submodel representations can be viewed as
  analogous to a process running in a multitasking operating system. \ There
  is a central queue which passes control to each agent in its queue in turn
  and, when given control, each agent runs for a time step. The machinery
  which supports this structure is the same for each representation, and the
  system is quite capable of running an arbitrary number of populations, and
  individuals at the same time.

  An alternative view is of the model as a whole as a sequence of mappings,
  <math|s<rsub|>:\<b-S\>\<rightarrow\>\<b-S\>>, each of which maps an element
  of of the state space, say <math|\<b-s\><rsub|t> \<in\>\<b-S\>>, to some
  other element, <math|<with|math-font-series|bold|s><rsub|t+\<delta\>t>>, in
  <math|\<b-S\>>. In all three representations the number of entities
  modelled remains the same, but in the mutating model the number of mappings
  applied at each time may change. \ This is a substantial formal difference
  and we will address it in the discussion at the end of the paper.\ 

  For the moment will will only consider the two ``pure'' models: the
  population-based representation and the individual-based representation. In
  these cases, each submodel would correspond to some subset of the ordinates
  in a state vector and the mapping <math|<wide|s|\<check\>><rsub|i>> or
  <with|mode|math|<wide|s|~><rsub|j>> would manipulate only the ordinates
  corresponding to the population <math|i> or the individual <math|j>. Thus,
  each of these mappings is pertinent only to a particular part of the state
  vector <math|<with|math-font-series|bold|s><rsub|t>>, and the update step
  corresponds to the mapping of the whole state from one step to the next.
  \ So we might write

  <\equation*>
    \<b-s\><rsub|t+\<delta\>t>= <wide|s|\<check\>><rsub|1>\<circ\>
    <wide|s|~><rsub|2>\<circ\> <wide|s|~><rsub|3>\<circ\><wide|s|\<check\>><rsub|4>\<ldots\>s<rsub|n>(\<b-s\><rsub|t>)
    \<equiv\> \<b-s\><rsub|t+\<delta\>t>=<left|(><big|odot><rsub|s<rsub|i>\<in\>\<cal-A\>>s<rsub|i><right|)>(\<b-s\><rsub|t>),
  </equation*>

  where each <math|s<rsub|i>> on the right hand side is either
  <math|<wide|s|\<check\>><rsub|i>> (for a population's update) or
  <math|<wide|s|~><rsub|i>> (for an individual's update). \ This update is
  performed for each time step until the defined end of the simulation. It is
  important to note that in these cases, the function composition is
  commutative: the individuals and the populations do not interact with each
  other, so the order of application isn't important.\ 

  These views of the system are compatible. Indeed, this duality lies at the
  heart of the mapping between the representations in this toy model.\ 

  The issue of whether an individual-based approach or a population-based
  approach is the best representation for modelling real situations really
  lies outside the scope of this paper, but for our purposes we will assume
  that (for whatever reason) the dynamics of the system which we wish to
  capture are best elicited by an individual-based representation. In the
  models which had a formative influence on this exploration,
  individual-based models were used to model the contact and uptake of
  contaminant. \ The primary reason for this is that in both of the earlier
  studies the initial exploration suggested that the structure of the plumes
  would be quite fine with a rapid decay from their peak values and that a
  Lagrangian approach might be more sensitive to the dynamics of the system.

  <subsection|Distributions and compatibility>

  The toy model has two basic representations of a group of organisms which
  proceed around a circular migratory path on an annual basis. \ Within one
  region which intersects this path there is a plume of some contaminant with
  an intensity which varies sinusoidally at a frequency which is relatively
  prime to the annual migration cycle. \ The contaminant load of the modelled
  entities is subject to constant decay, but when the group (or members of
  the group) are inside the boundary of the plume, their contaminant load
  increases. \ The processes of uptake and depuration are represented by an
  equation which relates the contaminant load at the end of the time step to
  their load at the beinging of the time step and the level of contact they
  experience over their path.

  The first representation of the group is as a set of individual agents
  whose movement is a directed random walk around the migratory circle. At
  each time step the trajectory of each indi<em|<em|>>vidual incorporates a
  directional component toward the ``target location'' on the migratory path
  which corresponds to the notional ideal centre of the group at that time.
  Each agent (instance of the submodel) maintains its own contaminant load,
  and its location and trajectory is independent of the others.\ 

  Our second approach is to represent the individuals as an aggregate entity.
  \ This is a mean-field representation of the population: we can, in some
  way, smear the individuals symmetrically around the disk to give us a
  representative entity which possesses a centroid and a radial distribution
  around that centroid which is consistent with the distribution of the
  individuals in the first representation. The density of the simulated
  individuals around their target location arising from their movement model
  closely follows the probability density function of a normal distribution.
  This is an ideal situation -- in practice there may be a number of
  environmental influences which make a such a simple population distribution
  unrealistic. The actual distribution of individuals is slightly skewed by
  their motions along the migratory circle, but we optimistically decide that
  the error introduced with the approximation is worth the resulting
  simplicity, particularly in light of the population's relatively slow
  motion around the migratory circle. From this representation of the
  individuals' density, we define the population's density,
  <with|mode|math|\<rho\>(p)>, to be the value of the standard 2D normal
  distribution with a mean of zero in each ordinate and a variance which is
  experimentally determined to match that of the individuals. More explicitly\ 

  <\equation*>
    \<rho\><with|mode|text|<math|<rsub|<rsub|>>(\|p\|) = S<rsub|<rsub|L>
    ><rsup|><frac|1|2\<pi\>\<sigma\><rsup|2>>exp<left|(>-<frac|p<rsub|x><rsup|2>
    + p<rsub|y><rsup|2>|2\<sigma\><rsup|2>><right|)>>,>
  </equation*>

  where <math|p =(p<rsub|x>, p<rsub|y>)> is the position in the population
  disk relative to its centre, <math|S<rsub|L>> is a scaling parameter (equal
  to 1.015) which ensures that the population density, <math|\<rho\>>,
  integrated over the finite disk with a radius, <math|\<tau\>>, is one, and
  <math|\<sigma\><rsup|2>> is the variance for the distribution. If the
  distribution of individuals around the point <math|m(t)> arising from the
  movement model and the distribution of the population around <math|m(t),
  which is identified with its centroid, >were not consistent, then the
  contaminant exposure of the two models would be incompatible and the sort
  of mutation error discussed above becomes significant. \ The effective
  radius, <math|\<tau\>>, of the population is statically ``tuned'' to
  reflect the observed distribution of the individual-based model. The
  variance in the distance of the individuals from <math|m(t)> was calculated
  and used as the variance for the population density. \ We took the value
  <math|\<tau\> = 9408.75> which was three standard deviations from the
  centre of the observed distribution.

  The general aim is to construct mappings between representations which
  preserve as much information as possible without undue burden. \ Clearly
  the significant information which the individual-based representation
  possesses and the population representation does not is the variation in
  contaminants amongst the individuals within the group. The role of the
  individual locations of these individuals is not really a significant
  factor for a large portion of the state-space since the interval between
  conversions from individuals to aggregates is large enough to ensure a
  thorough randomisation of relative position (though if the contaminant load
  exerted some influence on behaviour, such as the speed of the individual,
  this may not be so). We will discuss the construction of our third form of
  the system, the mutating model, in greater detail later.

  <section|Formulating the basic models>

  The basic representations play a dual role in the experiment. \ Not only do
  they provide comparison data in their own right, but each forms an
  unmodified component of the mutating model. This is accomplished by
  providing each of the basic models with enough extra machinery to report
  its state to a ``supervisor'' which manages the swap from one
  representation to another.\ 

  The population-based and individual-based models have been kept as similar
  as practicable in order to minimise the sources of divergence. \ Both
  models simulate migration around the same path and use the same contaminant
  uptake equations. \ Differences arise in the calculation of the contact
  with the contaminant, since the interactions of an individual moving
  through the plume is, in some sense, an integration across a path, while
  the population's expected interactions are more analogous to the
  integration over the domain of the population.\ 

  \;

  \;

  <subsection|The migratory circle and the plume>

  The migratory path is a circle which can be defined by the equation

  <\equation*>
    <with|math-font-series|bold|m>(t) = R \ \<cdot\>(cos( 2\<pi\>t/Y),
    sin(2\<pi\>t/Y)),
  </equation*>

  where <math|<with|math-font-series|bold|m>(t)>, denotes the ``location at
  time <math|t>'' on the migratory path, <math|R> is the radius of the
  migratory path and <math|Y> is the period. \ This circle is used for all
  the variant models of the system.

  The plume is modelled as an elliptical cloud. Its centroid is positioned on
  the migratory circle. The plume is modelled as an intensity at a location
  with an attenuation function. The intensity at a point,
  <with|math-font-series|bold|<with|mode|math|r>>, within the plume at time
  <with|mode|math|t> is given by\ 

  <\equation*>
    <with|mode|text|<with|mode|math|I(t,<with|math-font-series|bold|r>) =
    <frac|1|2>(1 + cos(2\<pi\>t/p))><with|mode|math|(\|<with|math-font-series|bold|r>-<with|math-font-series|bold|m>(t)\|<rsup|2>
    + (1 - <frac|\<less\>r,<with|math-font-series|bold|m>(t)\<gtr\>|\<less\>r,<with|math-font-series|bold|m>(t)\<gtr\>+1>)<rsup|0.01>)<rsup|-1>>,>
  </equation*>

  where <with|mode|math|p> is the period of the plume and <with|mode|math|r>
  is a distance from the source, and <math|\<less\><with|math-font-series|bold|a>,<with|math-font-series|bold|b>\<gtr\>
  \ = \|<with|math-font-series|bold|a>-<with|math-font-series|bold|b>\|>. The
  intensity is made asymmetric by instead taking
  <math|\<less\><with|math-font-series|bold|a>,<with|math-font-series|bold|b>\<gtr\>=
  \ (<with|math-font-series|bold|a>-<with|math-font-series|bold|b>) \<cdot\>
  (<sqrt|2|>, <frac|1|<sqrt|2>>)<rsup|<frac|1|2>>>. \ In the model's code,
  this intensity function is represented in cartesian coordinates rather than
  polar coordinates. \ The effective radius of the symmetric plume in the
  model is approximately 3684m, while the radius of the migratory circle,
  <math|R,> is 100000m and the period, <math|Y>, is 12 years.

  <subsection|Contaminant load and contact>

  The strategy for modelling the contact, uptake and depuration of our
  contaminant is the same for both representations. \ Initially a ``total
  contact'' is calculated for the time step. As might be expected, the total
  contact for a population is calculated in a different way to the total
  contact of an individual. \ In either case, this resulting contact is fed
  through the uptake-depuration o.d.e. \ 

  <\equation*>
    d C/d t = u M - \<lambda\>C
  </equation*>

  which is solved numerically for the value of <with|mode|math|C> given a
  contact mass, <math|M>, and an initial contaminant value or vector of
  values for <math|C>. Later, when we explore the time series resulting from
  the trials of the three representations, we will take <math|C<rsub|r k
  j>(t)> to represent the time series associated with individual <math|j> in
  trial <math|k> of representation <math|r>, <math|C<rsub|r k>(t)> to be the
  mean over all the individuals in the indicated representation and trial,
  and <math|C<rsub|r>(t)> denotes the time step generated by taking the mean
  of <with|mode|math|C<rsub|r k>(t)> across the values of <math|k> for the
  indicated representation.

  For individuals, the mass of contaminant which is available for uptake is
  taken to be the result of integrating the intensity of the plume over the
  path of the individual, <with|mode|math|\<b-P\><rsub|t>> to
  <with|mode|math|\<b-P\><rsub|t+\<delta\>>>,

  <\equation*>
    M =<big|int><rsub|\<b-P\><rsub|t><rsub|>><rsup|<with|math-font-series|bold|P><rsub|t+\<delta\><rsub|>>>I(<with|math-font-series|bold|p>)
    * \ \ \|\<b-p\>\| d <with|math-font-series|bold|p>
  </equation*>

  where we assume that the motion between
  <with|mode|math|(t,\<ell\><rsub|t>)> and <with|mode|math|(t +
  \<delta\><rsub|c>, l<rsub|t+\<delta\>>)> is linear. \ Populations are
  somewhat more complex. \ \ Here, we calculate the definite integral

  <\equation*>
    M = <big|int><rsub|\<b-P\><rsub|t><rsub|>><rsup|<with|math-font-series|bold|P><rsub|t+\<delta\>>>2<big|int><rsub|<with|math-font-series|bold|\<Omega\>>>I(<with|math-font-series|bold|p
    +><with|math-font-series|bold|\<omega\>>) *
    \<rho\>(<with|math-font-series|bold|p>
    +<with|math-font-series|bold|\<omega\>>) \ \ \ d
    <with|math-font-series|bold|\<omega\>> \ d
    <with|math-font-series|bold|p><rsup|<rsub|>>
  </equation*>

  where <with|mode|math|<with|math-font-series|bold|\<Omega\>>> is an area
  over which we assess the effective area of the population and
  <math|><math|\<omega\> +\<b-p\>> denotes the area
  <math|<with|math-font-series|bold|\<Omega\>>> translated so that its
  centroid corresponds to <math|\<b-p\>>. A fourth-order Runge-Kutta
  algorithm was used to solve the contact equation and a simple adaptive
  quadrature algorithm was used for calculating <math|M>.\ 

  <subsubsection|The homogeneous population model>

  The significant state variables for the population model consist of the
  time, a location, an effective radius and a contaminant load level. A
  population is effectively a radial density function, <math|\<rho\>>, for
  which <with|mode|math|<big|int><rsub|<with|math-font-series|bold|\<Omega\>>>\<rho\>(<with|math-font-series|bold|><with|math-font-series|bold|\<omega\>>)
  \ \ d <with|math-font-series|bold|\<omega\>><rsup|<rsub|>> = 1>; the disk
  we integrate over, <math|<with|math-font-series|bold|\<Omega\>>,> moves
  uniformly with its centre on migratory circle,
  <math|<with|math-font-series|bold|m>(t)>, in accord with the annual
  migration. The vector of contaminant loads (possibly empty or only a single
  quantity) is updated at each time step in the same way as the individuals
  according to the contact during that interval, and the uptake and
  depuration rates. \ The populations are also given a nominal number of
  members, though this datum is not useful unless we are considering a
  mutating model.\ 

  <subsubsection|The homogeneous individual-based model>

  Individuals are basically represented by a location, a subjective time, a
  velocity vector and a contaminant load. \ There are obviously other
  parameters which influence their contaminant load: rates of uptake and
  depuration, and the parameters which condition their drunkard's walk around
  the migratory circle. \ In our case, these attributes really only
  contribute to the rate of progress<verbatim|> around the migratory circle
  (the frequency and duration of contact) and the distribution of the spatial
  coverage of the group of individuals around
  <math|<with|math-font-series|bold|m>(t)>, and hence the effective radius of
  the population. \ So, the details of the mechanics of the movement of the
  individuals are also relevant only in so far as they help inform the
  construction of the population model. \ 

  <section|The mutating model>

  The mutating model tests our hypothesis: that that it may be more efficient
  in terms of time or representational accuracy to change the representation
  of components of the model, depending on the nature of the region of the
  state-space a submodel. The specific example we are dealing with,
  individuals interacting with a highly local plume, is very simple -- almost
  simplistic -- but it possesses many of the properties which the general
  problem of an <em|ad hoc> animal/plume interaction might be expected to
  exhibit, namely the test for coincidence, generating a random walk for each
  individual, and evaluating the uptake and decay of the contaminant load.
  Over a long period of infrequent contact, we would expect quite a lot of
  time to be spent generating random walks and evaluating the contact and
  uptake in places where there is no likelihood of contact at all. \ Our
  strategy is to adopt, over particular regions in the state space,
  appropriate representations which preserve enough fidelity in the basic
  properties of the system to reconstitute any missing data required for
  another representation without incurring undue error or operational
  overhead. \ This is, fundamentally, about managing the often competing
  demands of representational accuracy, mathematical tractibility, and
  run-time.

  In order to test the hypothesis, each of the possible representations the
  mutating model may assume is identical to one or the other basic,
  ``homogeneous'' representations. \ In the case of the mutating model's
  population-based representation, the contaminant value (a 1-tuple) is
  replaced by a vector consisting of the contaminant loads of the individuals
  with non-zero contaminant levels, the levels of the remaining members of
  the aggregate represented by the population are assumed to be zero. \ The
  values in each ordinate of the <math|n>-tuple are decayed independently as
  time progresses.

  Individuals are mapped into a population by adding their contaminant level
  to the contaminant vector of an appropriate population after their movement
  and contaminant decay have been calculated. \ When a population-based model
  is mapped back to an individual-based representation, the non-zero
  contaminant levels are first mapped onto new individuals whose random
  locations are chosen to be consistent with the distribution for the
  population around its centroid. This mapping preserves the contaminant load
  present in the system and the likelihood of an encounter between a
  contaminated individual and new contaminants. \ In this way the distinct
  contamination levels within the system are maintained with a minimum of
  cost. \ 

  Some overhead is incurred in managing the swap from one representation to
  another. \ The actual conversion is done by a routine which is integrated
  with the scheduler which manages the transition from one time-step to the
  next. Each of the models is able to indicate to the scheduler that a change
  in representation may be reasonable when it determines that it has left its
  domain of efficiency. \ 

  <subsection|Heuristics: When do we swap>

  The contraints we impose on this toy model are quite stringent so the
  heuristics we can use to decide when to change representation may thus
  assume a great deal more than might be the case in a general situation: we
  know where the contaminants are at all times, the movement of the
  populations and the individuals are controlled, and the behaviour of the
  simulated organisms does not change through the run. In the general case,
  the heuristics might be much more complex and require a great deal more
  data. Deciding when to change the representation is central to the
  optimisation of the system: changing at the wrong time might sacrifice
  accuracy, efficiency or both.

  In our case, the principal issue is whether or not an agent
  (population-based or individual-based) may encounter contaminants: in our
  case it is simple, there is a single source and the mean movement toward or
  away from that source is quite predictable. \ As a population enters the
  region of contaminants we can immediately determine that it will encounter
  contaminants quite consistently for some non-trivial period, and a mapping
  is called for.

  The which determines if an individual is mapped into a population is
  similarly straightforward: an individual is mapped into a population if it
  is far enough outside the region of contaminants that there is no chance
  that it might encounter them in the next few time-steps, and it is close
  enough to a population. If there is no suitable population, a new one is
  created in order to accomodate it. This spatial constraint is stringent
  enough, when combined with the speed of migration, that the system hasn't
  needed to create more than one population in any of the annual cycles in
  the trials. This fortunate state of affairs is largely due to strong
  contstraints on the movement associated with the annual trek around the
  migratory circle.

  The transition rules in this model, from population to individual or from
  individual to population, can be based solely on the distance to the
  contaminant source, the time-step and the speed at which the simulated
  group of organisms moves around its annual path. \ Our heuristic could be
  generalised to systems where there were multiple, dynamically instanciated
  sources, when the sources of contamination could be determined at the
  beginning of a time-step. \ This particular model attempts to present a
  greatly simplified version of the general case -- if the hypothesis were to
  fail in this case it would be reasonable to assume that it would fail in
  most cases.\ 

  <subsection|Sensitivity to plume distribution><label|Plume1>

  The issue of whether the configuration of the plume has an different effect
  on the results of a simulation depending on the representation of the
  organisms is important. It is easy to imagine calibrating a
  population-based representation with data from fine-scale individual
  simulations in order to make the most of the economies a population-based
  model affords, but this may not be a good idea if there is poor
  correspondence between the two models over their domain.

  Additional trials of the standard (symmetric) plume model were run to
  extend the data available, and a corresponding series of trials with an
  asymmetric plume were run for comparison. \ Our analysis in these trials is
  based on a total of eighty trials of forty individuals with each trial
  covering twelve years. The asymmetric plume covers the same area as the
  symmetric plume and the integral over the area of the plumes are the same,
  up to the numerical error in the quadrature.

  <section|Results>

  The analysis in this section is based on three sets of runs. \ The first
  set is comprised of forty trials of each of the model representations
  (purely individual, mutating and population based). \ Each trial tracked
  either forty individuals or a population which nominally represented forty
  individuals. \ The contaminant plume was symmetric and its centroid was on
  the migratory circle. \ This set of runs was conducted to establish the
  relative cost in cpu-time of the three approaches and to experimentally
  verify (at least to some degree) the equiuvalence which ought to exist
  between the purely individual-based model and the mutating model. \ The
  second trial consisted of another set of forty replicates of the mutating
  model, mirroring the mutating trials in the first set. \ The third set
  consisted of eighty trials of the mutating model, and one of the population
  mode. \ The principal difference between the second and third set is that
  the second, when combined with the first gives us eighty replicates using a
  symmetric plume, while the third set replaces the plume with an asymmetric
  plume which is tangential to the migratory circle. \ Our initial analysis
  will only consider the trials in the first set of runs, and a comparison of
  the reponses to the asymmetric plume will be treated separately.

  Both population-based and individual-based representations may use a
  heuristic which suppresses contact evaluation when they are sufficiently
  far from the contaminant source. No other significant optimisations of the
  code have been made, and as far as possible the same routines are used in
  both representations. \ The motivation for this approach is to provide a
  common baseline which we can use to measure the utility of the adaptive
  representation. \ 

  To ensure that run-time comparisons are meaningful all of the simulations
  in the first set of trials were run on the same computer. Each of the
  configurations simulated forty organisms for twelve years.

  <subsection|Contaminant load correspondence between representations>

  The set of trajectories arising from the various representations aren't
  directly comparable. \ Our individual-based representation produces a time
  series of contaminant levels for each individual, while the
  population-based model produces a ``mean load'' across the whole group of
  entities it is representing. \ The mutating model sits between the two,
  sometimes producing individual time series and sometimes mean time series
  for varying parts of the population. \ In order to compare the dynamics of
  the system we generate mean time series for each of the <math|k> trials in
  the individual-based and mutating sets, \ <math|<rsup|>C<rsub|i k>(t)> and
  <math|<rsup|>C<with|math-font-series|bold|><rsub|m k>(t)>, paying special
  attention to generating the correct mean in the mutating model from time
  steps which have a mixture of individual trajectories and mean
  trajectories. \ Each of these mean time series corresponds to the mean
  contaminant load of the population, <math|C<rsub|p>(t)>, produced by the
  population-based model; averaging them -- constructing <math|C<rsub|r>(t) =
  <frac|1|k>\<Sigma\><rsup|k><rsub|j=1>C<rsub|r j>(t)>,<with|mode|math| where
  ><math|r> is one of <math|i> or <math|m> -- is equivalent to running many
  stochastic trials and averaging to fit the population model.

  Using <math|<rsup|>C<rsub|i k>(t)>, <math|<rsup|>C<rsub|m k>(t)> and
  <math|<rsup|>C<rsub|p>(t)> we find the maximum value attained for each
  representation, <math|<wide|C|^><rsub|r>>. We are also interested in the
  mean value across time of each representation,<hspace|\<htab\|0\>>
  <math|<wide|C|\<bar\>><rsub|r> = <frac|1|T>\<Sigma\><rsub|t\<in\>T>><math|
  C<rsub|r>(t)> \ These quantities are listed in Table
  <reference|MaximaMeans>.

  <\big-table>
    <tabular*|<tformat|<twith|table-lborder|2>|<twith|table-rborder|2>|<twith|table-tborder|2>|<twith|table-bborder|3>|<table|<row|<cell|Time
    series>|<cell|<math|<wide|C|^><rsub|r>>>|<cell|<math|<wide|C|\<bar\>><rsub|r>>>>|<row|<cell|<math|C<rsub|i>>>|<cell|<with|mode|math|0.1787>>|<cell|<with|mode|math|0.0390>>>|<row|<cell|<math|C<rsub|m>>>|<cell|<with|mode|math|0.1821>>|<cell|<with|mode|math|0.0392>>>|<row|<cell|<math|C<rsub|p>>>|<cell|<with|mode|math|0.1387>>|<cell|<with|mode|math|0.0350>>>>>>

    \;
  </big-table|Maxima and Means<label|MaximaMeans>>

  <subsection|Contaminant load variability><label|LoadVar>

  Several measures of variability in the time series were calculated using
  the aggregated time series <math|<rsup|>C<rsub|i k>(t)> and
  <math|<rsup|>C<rsub|m k>(t)> and their respective means across the <math|k>
  trials, namely <math|C<rsub|i>(t)> and <math|C<rsub|m>(t)>. We'll take\ 

  <\equation*>
    <with|mode|text|<math|<wide|\<sigma\>|^><rsub|a b>=
    max<rsub|t\<in\>[1,T]><left|[><frac|1|k><big|sum><rsub|j =
    1><rsup|k>(C<rsub|a k>(t) - C<rsub|b>(t))<rsup|2><right|]><rsup|1/2>>>
  </equation*>

  and

  <\equation*>
    \<sigma\><rsub|a \ b> =<left|[><frac|1|T><big|sum><rsub|t=1><rsup|T><left|[><frac|1|k><big|sum><rsub|j
    = 1><rsup|k>(C<rsub|a \ k>(t) - C<rsub|b>(t))<rsup|2><right|]><right|]><rsup|1/2>,
  </equation*>

  where <math|T> is the total number of timesteps taken. \ Clearly we can
  write <math|\<sigma\><rsub|r r>> as <math|\<sigma\><rsub|r>> without
  introducing ambiguity, and similarly for <math|<wide|\<sigma\>|^><rsub|r>>.
  The values for these measure of of variability are presented in Table
  <reference|LoadVarTbl>.

  <\big-table>
    <tformat|<cwith|1|-1|1|-1|cell-halign|c>|<tabular*|<tformat|<twith|table-lborder|2>|<twith|table-rborder|2>|<twith|table-tborder|2>|<twith|table-bborder|3>|<table|<row|<cell|StdDev>|<cell|<math|C<rsub|i>>>|<cell|<math|C<rsub|m>>>|<cell|<math|C<rsub|p>>>>|<row|<cell|<math|<wide|\<sigma\><rsub|>|^><rsub|i>>>|<cell|<with|mode|math|0.0083>>|<cell|<with|mode|math|0.0084>>|<cell|0.0534>>|<row|<cell|<math|\<sigma\><rsub|i>>>|<cell|0.0024>|<cell|<with|mode|math|0.0024>>|<cell|<with|mode|math|0.0096>>>|<row|<cell|<math|<wide|\<sigma\><rsub|>|^><rsub|m>>>|<cell|<with|mode|math|0.0090>>|<cell|<with|mode|math|0.0090>>|<cell|0.0538>>|<row|<cell|<math|\<sigma\><rsub|m>>>|<cell|0.0024>|<cell|<with|mode|math|0.0024>>|<cell|<with|mode|math|0.0096>>>>>>><hspace|\<htab\|0\>><hspace|\\hfill>
  <|big-table>
    Deviations amongst the model runs

    with respect to a given mean<label|LoadVarTbl>\ 

    \;
  </big-table>

  <subsection|Sensitivity to asymmetry in the plume>

  We will use the same notation as Section <reference|LoadVar> for the data
  derived from the symmetric plumes, while we will add a prime symbol to the
  data derived from the asymmetric plumes. \ Thus, the mean value time series
  for the mutating model would be denoted <math|C<rprime|'><rsub|m>> and the
  mean value of that time series is <math|<wide|C|\<bar\>><rprime|'>>.

  The data for the symmetric plume is presented in Table <reference|Symplume>
  and the data for the asymmetric plume is presented in Table
  <reference|Asymplume>.

  \ <big-table|<tabular*|<tformat|<table|<row|<cell|<subtable|<tformat|<table|<row|<cell|<tabular*|<tformat|<twith|table-lborder|2>|<twith|table-rborder|2>|<twith|table-tborder|2>|<twith|table-bborder|3>|<table|<row|<cell|Time
  series>|<cell|<math|<wide|C|^><rsub|r>>>|<cell|<math|<wide|C|\<bar\>><rsub|r>>>>|<row|<cell|<math|C<rsub|m>>>|<cell|<with|mode|math|0.1738>>|<cell|<with|mode|math|0.0392>>>|<row|<cell|<math|C<rsub|p>>>|<cell|<with|mode|math|0.1387>>|<cell|<with|mode|math|0.0350>>>>>>>>>>>>|<cell|<subtable|<tformat|<table|<row|<cell|<tabular*|<tformat|<twith|table-lborder|2>|<twith|table-rborder|2>|<twith|table-tborder|2>|<twith|table-bborder|3>|<table|<row|<cell|>|<cell|StdDev>|<cell|<math|C<rsub|m>>>|<cell|<math|C<rsub|p>>>>|<row|<cell|>|<cell|<math|<wide|\<sigma\><rsub|>|^><rsub|m>>>|<cell|<with|mode|math|0.0088>>|<cell|0.0535>>|<row|<cell|>|<cell|<math|\<sigma\><rsub|m>>>|<cell|<with|mode|math|0.0024>>|<cell|<with|mode|math|0.0098>>>>>>>>>>>>>>>>|Symmetric
  plume results<label|Symplume>>

  <\big-table|<tabular*|<tformat|<table|<row|<cell|<subtable|<tformat|<table|<row|<cell|<tabular*|<tformat|<twith|table-lborder|2>|<twith|table-rborder|2>|<twith|table-tborder|2>|<twith|table-bborder|3>|<table|<row|<cell|Time
  series>|<cell|<math|<wide|C<rprime|'>|^><rsub|r>>>|<cell|<math|<wide|C<rprime|'>|\<bar\>><rsub|r>>>>|<row|<cell|<math|C<rprime|'><rsub|m>>>|<cell|<with|mode|math|0.1856>>|<cell|<with|mode|math|0.0394>>>|<row|<cell|<math|C<rprime|'><rsub|p>>>|<cell|<with|mode|math|0.1763>>|<cell|<with|mode|math|0.0445>>>>>>>>>>>>|<cell|<subtable|<tformat|<table|<row|<cell|<tabular*|<tformat|<twith|table-lborder|2>|<twith|table-rborder|2>|<twith|table-tborder|2>|<twith|table-bborder|3>|<table|<row|<cell|>|<cell|StdDev>|<cell|<math|C<rprime|'><rsub|m>>>|<cell|<math|C<rprime|'><rsub|p>>>>|<row|<cell|>|<cell|<math|<wide|\<sigma\><rsub|>|^><rsub|m><rprime|'>>>|<cell|<with|mode|math|0.0087>>|<cell|0.0616>>|<row|<cell|>|<cell|<math|\<sigma\><rsub|m><rprime|'>>>|<cell|<with|mode|math|0.0025>>|<cell|<with|mode|math|0.0092>>>>>>>>>>>>>>>>>
    Asymmetric plume results<label|Asymplume>
  </big-table>

  <subsection|Run-time>

  Each run collected data regarding the amount of time spend in different
  parts of the model. \ As a basis of comparison, the overall amount of time
  spent running on the cpu (linux/unix ``cpu seconds'') for the whole run is
  the most important figure, though the time spent in other parts provipdes
  illumination into just where the effort is concentrated. \ Predictably,
  most of the effort is in calculating contact and updating contaminant
  loads.

  The optimisation of supressing the contact calculations when a population
  is outside the area of potential contact seemed to make very little
  difference to the run-time of population model (of the order of 3%), and
  seems unlikely to make a great deal of difference to the mutating model.
  \ In the case of the purely individual-based model, this sort of
  optimisation is likely to play a much bigger role in that a three second
  penalty would be multiplied by the number of entities simulated. \ 

  The population-based model ran for 98.7. This model is deterministic and
  the amount of cpu time used is very stable, so only a single run is
  considered for comparison. The purely individual-based models took just
  over a mean time of 4205 cpu seconds with a standard deviation of barely
  more than 16 seconds and the mean of the mutating model's run time was 1157
  cpu seconds with a standard deviation of slightly over 11 seconds.\ 

  <section|What the toy model tells us>

  The initial sets of runs have two aims, namely to establish the relative
  speeds of the three approaches and to support the assertion that the
  mutating model possesses the same essential dynamics as the purely
  individual-based representation. \ The essential equivalence of the
  individual-based and mutating representations is well supported and there
  is a significant benefit in run-time with the mutating model when compared
  to the individual-based model. \ The population model, while fastest by
  several orders of magnitude, showed responses to the contaminant plume
  which were significantly different to those of the other two models.

  <subsection|Means, maxima and variation>

  In terms of the formulation of the mutating model, it is difficult to see
  how there could be a significant difference between it and the purely
  individual-base model, at least in terms of the contaminant loads in the
  group of individuals. The means, maxima and the various calculations of
  deviation between the individual-based and mutating models were very close
  and this supports the belief that they can be treated as essentially
  identical in terms of modelling the contaminant load in the group of
  simulated organisms. \ Both of these representations, however, differed
  noticably from the population-based model; \ the population's mean value,
  <math|<wide|C|\<bar\>><rsub|p>>, was about 10% lower than that of the
  either <math|<wide|C|\<bar\>><rsub|i>> or <math|<wide|C|\<bar\>><rsub|m>>,
  and the root mean square error of either the individual-based model and the
  mutating model from the population's mean value was about four times
  greater than from either of the individual-based or mutating means. \ 

  The discrepancy between the population-based uptake and individual-based
  uptake is enough to suggest that the formulation of the population
  distribution was not quite consistent with the spatial dynamics of the
  individuals, that the tuning of the population model with respect to the
  individuals was inadequate or, most likely, that both of these errors
  contribute to the difference. This is the sort of situation alluded to in
  section <reference|Plume1>: the fundamental models exhibit different
  dynamics over the domain of the simulation, and this is one of the niches
  which a mutating approach might be most advantageous. \ It is difficult to
  accurately describe a wild population's distribution across the whole of
  its domain -- changes in the behaviour of individuals (arising from
  predation, drought or other stressors) may engender quite different
  population distributions, so even in the case of a purely population-based
  representation an argument can be made for changing the underlying
  representation based on the local state.

  <subsection|Run-time>

  The population-based model is clearly much faster than either of the other
  two representations, and for situations where we can reasonably make
  mean-field assumptions and our distribution model fits well with the
  observed data it is likely that the economies afforded by the speed of
  execution and the well established mathematical understanding of such
  models will outweigh benefits of individual-based representations. \ In
  situations where mean-field assumptions are not tenable it is similarly
  clear that there are benefits to using mutating models. \ Purely
  individual-based models entail significant overhead, which in our context
  contributes nothing to the outcome. \ In a richer environment with diverse
  responses to the local state, an individual-based model's `non-productive'
  overhead may be competitive with the overheads associated with the
  management and transition of mutating models.

  <subsection|Symmetry and asymmetry>

  The data in the second set of trials had means, maxima and deviations for
  the mutating model which were largely between the symmetric and asymmetric
  contaminant fields. This matches our expectations since the total areas
  covered, the total contaminant load over the area are the same for the two
  representations are the same, and the speed of the agents through the plume
  is relatively slow. In contrast, the symmetric and asymmetric trials of the
  population-based model had markedly different maxima and mean values
  through time. \ 

  The data suggest that the individual-based representation is reasonably
  robust with respect to the plume shape (at least in the context of this
  experiment). The population model did not behave as expected and it may be
  that the model of the distribution of individuals which formed the basis of
  the population model may have systematic problems with the population wide
  sampling of the plume -- the increase in mean and maximum levels when the
  plume was asymmetric and oriented tangentially along the track of the
  population suggests that the centre of the population is oversampled. The
  implications of this are that we must choose our population distribution
  carefully, and a simple gaussian distribution about a centroid may not
  behave well with respect to the plume dynamics.\ 

  Set against this, the numeric modelling of populations is well established.
  It is quite likely that the individual-based model would fail to meet
  expectations with regard to the dynamics of recruitment and mortality, had
  those aspects of the life history of a population been included in the
  model. \ In such a situation, the mutating model would have performed best
  overall -- largely maintaining the fidelity of each of the representations
  across the state-space.

  <section|Discussion>

  In this section, we move from the specific to the general: having
  established that there is a <em|prima facie> case for mutating models, we
  discuss some of the issues which have arisen. Loosely speaking, we will
  group our discussion in terms of <em|formal notation>, <em|optimisation>,
  <em|transition heuristics>, and <em|model transitions>.

  The formal notation is presented as a means of aiding the discussion, and
  to begin to establish some sort of mathematical basis for the strategy. In
  itself it contributes nothing but clarity, and, with luck, a little
  illumination. \ 

  <subsection|Formal notation>

  We can consider many simulation models to be the repeated application of a
  chain of function compositions, and indeed adherents of the
  functional-programming paradigm would regard this as the natural approach.
  Each application takes the model from the state associated with time
  <math|t> to its next state at <math|t+\<delta\>t>. Many of the approaches
  to modelling ecosystems can be dealt with in this way (cellular automata,
  variable speed splitting models, and classical individual-based models to
  name a few). \ One of the aspects which makes formalising the mutating
  model more complex is that not only may the number of functions in our
  chain change, but the functions themselves may change, but we are fortunate
  that we can choose to consider only finite systems, such as computer based
  models.

  Let us consider the aggregate model and its domain: in principle this
  corresponds to what happens (or might happen) in some patch of the real
  world. So, we let <math|G> be a function on the state-space
  <math|<with|math-font-series|bold|S>> where
  <math|G(<with|math-font-series|bold|s><rsub|t>) =
  <with|math-font-series|bold|s><rsub|t+\<delta\>t>
  \<forall\><with|math-font-series|bold|s>\<in\><with|math-font-series|bold|S>>.
  \ Already we find that our notation is too sparse: just a moment ago we
  indicated that the form of <math|G> changes with respect to its location in
  the state space. So, we refine things just a little. \ Let
  <math|<with|math-font-series|bold|F>> be the set of all functions which map
  elements of <math|<with|math-font-series|bold|S>> onto another element of
  <math|<with|math-font-series|bold|S>>, and let
  <math|\<Gamma\>(<with|math-font-series|bold|s>, g) = h> for <math|g, h
  \<in\> <with|math-font-series|bold|F>, <with|math-font-series|bold|s>\<in\><with|math-font-series|bold|S>>.

  Then in a straightforward model we might write a typical update step as\ 

  <\equation*>
    G(<with|math-font-series|bold|s><rsub|t>) =
    \ <left|(><big|odot><rsub|i=1><rsup|n>g<rsub|i><right|)> =
    <with|math-font-series|bold|s><rsub|t+\<delta\>t>
  </equation*>

  where <math|n> is the number of entities in the aggregate model, and
  <math|g<rsub|i>\<in\><with|math-font-series|bold|F>> which is (probably)
  associated with the ordinates of the state space associated with one
  submodel, though it ought to be noted that the an entity may represent more
  than one individual or facet of the simulation. With this notion in place,
  we can recast it as\ 

  <\equation*>
    <left|(>\<Gamma\>(<with|math-font-series|bold|s><rsub|t>,G<rsub|t-\<delta\>t>)
    <right|)>(<with|math-font-series|bold|s><rsub|t>) =
    G<rsub|t>(<with|math-font-series|bold|s><rsub|t>) =
    \ <left|(><big|odot><rsub|i=1><rsup|n>g<rsub|i><right|)> =
    <with|math-font-series|bold|s><rsub|t+\<delta\>t>.
  </equation*>

  This version makes explicit the dependence of <math|G<rsub|t>> on the
  state, <math|<with|math-font-series|bold|s><rsub|t>>, and the
  representation in the previous step, and it should be observed that the
  value of <math|n> depends largely on where <math|\<Gamma\>> maps
  <with|mode|math|(<with|math-font-series|bold|s><rsub|t>,G<rsub|t-\<delta\>t>)>.
  \ 

  \;

  if we know that the composition of these functions, denoted by
  <math|\<odot\>>, is commutative. \ In order to preserve the commutativity
  of the component mappings, we introduce the mutation step as a mapping
  <math|\<cal-M\>(<with|math-font-series|bold|s><rsub|t>,\<cal-A\><rsub|t>)
  =\<cal-A\><rsub|t+\<delta\>t>> where <math|\<cal-M\>> is a function used to
  map a set of submodels which spans the the state space of the simulation to
  another spanning set of submodels based on both the present state of the
  model as a whole and on the collection of submodels in
  <math|\<cal-A\><rsub|t>>. \ This mapping corresponds to the activity of the
  scheduler in the model.

  \;

  \;

  \;

  In our simple example of motion above, both models require the same data:
  the object's mass, location, velocity and acceleration. \ This set of
  variables is adequate to represent any state in either model: a non-trivial
  model should consider a system which has model representations with
  different state spaces. The transition from one state-space to another
  which is associated with the change in representation may be introduce
  error either from the loss of particular attributes or from the assumption
  of some ``default'' value for an attribute which was not previously
  present.\ 

  \;

  \;

  <section|Conclusion>

  The mutating model can be viewed from either direction -- as a population
  model which converts itself to an individual-based representation when
  contaminants are encountered, or as an individual-based model which changes
  into a population when the specific locations of the individuals become
  irrelevant. The most challenging parts of the task are to determine what
  information must be maintained in each of the representations to ensure
  that the migration from one formulation to another doesn't introduce
  unreasonable error, and to control the migration between forms with a
  minimum of overhead.

  This simple model demonstrated that changing the representation of a system
  from one form to another can provide a mechanism for increasing the
  efficiency or accuracy with only a little extra effort. \ A more
  challenging avenue for study would be a similar model which included
  population recruitment and mortality -- particularly if there were
  sub-lethal effects associated with the contaminant load which changed the
  behaviour or fecundity of the simulated organisms. \ 
</body>

<\initial>
  <\collection>
    <associate|preamble|false>
  </collection>
</initial>

<\references>
  <\collection>
    <associate|Asymplume|<tuple|4|?>>
    <associate|LoadVar|<tuple|5.2|?>>
    <associate|LoadVarTbl|<tuple|2|?>>
    <associate|MaximaMeans|<tuple|1|?>>
    <associate|Plume1|<tuple|4.2|?>>
    <associate|Symplume|<tuple|3|?>>
    <associate|auto-1|<tuple|1|1>>
    <associate|auto-10|<tuple|3.2.2|5>>
    <associate|auto-11|<tuple|4|5>>
    <associate|auto-12|<tuple|4.1|6>>
    <associate|auto-13|<tuple|4.2|6>>
    <associate|auto-14|<tuple|5|6>>
    <associate|auto-15|<tuple|5.1|7>>
    <associate|auto-16|<tuple|1|7>>
    <associate|auto-17|<tuple|5.2|7>>
    <associate|auto-18|<tuple|2|?>>
    <associate|auto-19|<tuple|5.3|?>>
    <associate|auto-2|<tuple|1.1|1>>
    <associate|auto-20|<tuple|3|?>>
    <associate|auto-21|<tuple|4|?>>
    <associate|auto-22|<tuple|5.4|?>>
    <associate|auto-23|<tuple|6|?>>
    <associate|auto-24|<tuple|6.1|?>>
    <associate|auto-25|<tuple|6.2|?>>
    <associate|auto-26|<tuple|6.3|?>>
    <associate|auto-27|<tuple|7|?>>
    <associate|auto-28|<tuple|7.1|?>>
    <associate|auto-29|<tuple|8|?>>
    <associate|auto-3|<tuple|2|2>>
    <associate|auto-4|<tuple|2.1|2>>
    <associate|auto-5|<tuple|2.2|3>>
    <associate|auto-6|<tuple|3|4>>
    <associate|auto-7|<tuple|3.1|4>>
    <associate|auto-8|<tuple|3.2|4>>
    <associate|auto-9|<tuple|3.2.1|5>>
  </collection>
</references>

<\auxiliary>
  <\collection>
    <\associate|table>
      <tuple|normal|Maxima and Means<label|MaximaMeans>|<pageref|auto-16>>

      <\tuple|normal>
        Deviations amongst the model runs

        with respect to a given mean<label|LoadVarTbl>\ 

        \;
      </tuple|<pageref|auto-18>>

      <tuple|normal|Symmetric plume results<label|Symplume>|<pageref|auto-20>>

      <\tuple|normal>
        Asymmetric plume results<label|Asymplume>
      </tuple|<pageref|auto-21>>
    </associate>
    <\associate|toc>
      <vspace*|1fn><with|font-series|<quote|bold>|math-font-series|<quote|bold>|1<space|2spc>Introduction>
      <datoms|<macro|x|<repeat|<arg|x>|<with|font-series|medium|<with|font-size|1|<space|0.2fn>.<space|0.2fn>>>>>|<htab|5mm>>
      <no-break><pageref|auto-1><vspace|0.5fn>

      <with|par-left|<quote|1.5fn>|1.1<space|2spc>Scope
      <datoms|<macro|x|<repeat|<arg|x>|<with|font-series|medium|<with|font-size|1|<space|0.2fn>.<space|0.2fn>>>>>|<htab|5mm>>
      <no-break><pageref|auto-2>>

      <vspace*|1fn><with|font-series|<quote|bold>|math-font-series|<quote|bold>|2<space|2spc>Experimentation>
      <datoms|<macro|x|<repeat|<arg|x>|<with|font-series|medium|<with|font-size|1|<space|0.2fn>.<space|0.2fn>>>>>|<htab|5mm>>
      <no-break><pageref|auto-3><vspace|0.5fn>

      <with|par-left|<quote|1.5fn>|2.1<space|2spc>Basic model structure
      <datoms|<macro|x|<repeat|<arg|x>|<with|font-series|medium|<with|font-size|1|<space|0.2fn>.<space|0.2fn>>>>>|<htab|5mm>>
      <no-break><pageref|auto-4>>

      <with|par-left|<quote|1.5fn>|2.2<space|2spc>Distributions and
      compatibility <datoms|<macro|x|<repeat|<arg|x>|<with|font-series|medium|<with|font-size|1|<space|0.2fn>.<space|0.2fn>>>>>|<htab|5mm>>
      <no-break><pageref|auto-5>>

      <vspace*|1fn><with|font-series|<quote|bold>|math-font-series|<quote|bold>|3<space|2spc>Formulating
      the basic models> <datoms|<macro|x|<repeat|<arg|x>|<with|font-series|medium|<with|font-size|1|<space|0.2fn>.<space|0.2fn>>>>>|<htab|5mm>>
      <no-break><pageref|auto-6><vspace|0.5fn>

      <with|par-left|<quote|1.5fn>|3.1<space|2spc>The migratory circle and
      the plume <datoms|<macro|x|<repeat|<arg|x>|<with|font-series|medium|<with|font-size|1|<space|0.2fn>.<space|0.2fn>>>>>|<htab|5mm>>
      <no-break><pageref|auto-7>>

      <with|par-left|<quote|1.5fn>|3.2<space|2spc>Contaminant load and
      contact <datoms|<macro|x|<repeat|<arg|x>|<with|font-series|medium|<with|font-size|1|<space|0.2fn>.<space|0.2fn>>>>>|<htab|5mm>>
      <no-break><pageref|auto-8>>

      <with|par-left|<quote|3fn>|3.2.1<space|2spc>The homogeneous population
      model <datoms|<macro|x|<repeat|<arg|x>|<with|font-series|medium|<with|font-size|1|<space|0.2fn>.<space|0.2fn>>>>>|<htab|5mm>>
      <no-break><pageref|auto-9>>

      <with|par-left|<quote|3fn>|3.2.2<space|2spc>The homogeneous
      individual-based model <datoms|<macro|x|<repeat|<arg|x>|<with|font-series|medium|<with|font-size|1|<space|0.2fn>.<space|0.2fn>>>>>|<htab|5mm>>
      <no-break><pageref|auto-10>>

      <vspace*|1fn><with|font-series|<quote|bold>|math-font-series|<quote|bold>|4<space|2spc>The
      mutating model> <datoms|<macro|x|<repeat|<arg|x>|<with|font-series|medium|<with|font-size|1|<space|0.2fn>.<space|0.2fn>>>>>|<htab|5mm>>
      <no-break><pageref|auto-11><vspace|0.5fn>

      <with|par-left|<quote|1.5fn>|4.1<space|2spc>Heuristics: When do we swap
      <datoms|<macro|x|<repeat|<arg|x>|<with|font-series|medium|<with|font-size|1|<space|0.2fn>.<space|0.2fn>>>>>|<htab|5mm>>
      <no-break><pageref|auto-12>>

      <with|par-left|<quote|1.5fn>|4.2<space|2spc>Sensitivity to plume
      distribution <datoms|<macro|x|<repeat|<arg|x>|<with|font-series|medium|<with|font-size|1|<space|0.2fn>.<space|0.2fn>>>>>|<htab|5mm>>
      <no-break><pageref|auto-13>>

      <vspace*|1fn><with|font-series|<quote|bold>|math-font-series|<quote|bold>|5<space|2spc>Results>
      <datoms|<macro|x|<repeat|<arg|x>|<with|font-series|medium|<with|font-size|1|<space|0.2fn>.<space|0.2fn>>>>>|<htab|5mm>>
      <no-break><pageref|auto-14><vspace|0.5fn>

      <with|par-left|<quote|1.5fn>|5.1<space|2spc>Contaminant load
      correspondence between representations
      <datoms|<macro|x|<repeat|<arg|x>|<with|font-series|medium|<with|font-size|1|<space|0.2fn>.<space|0.2fn>>>>>|<htab|5mm>>
      <no-break><pageref|auto-15>>

      <with|par-left|<quote|1.5fn>|5.2<space|2spc>Contaminant load
      variability <datoms|<macro|x|<repeat|<arg|x>|<with|font-series|medium|<with|font-size|1|<space|0.2fn>.<space|0.2fn>>>>>|<htab|5mm>>
      <no-break><pageref|auto-17>>

      <with|par-left|<quote|1.5fn>|5.3<space|2spc>Sensitivity to asymmetry in
      the plume <datoms|<macro|x|<repeat|<arg|x>|<with|font-series|medium|<with|font-size|1|<space|0.2fn>.<space|0.2fn>>>>>|<htab|5mm>>
      <no-break><pageref|auto-19>>

      <with|par-left|<quote|1.5fn>|5.4<space|2spc>Run-time
      <datoms|<macro|x|<repeat|<arg|x>|<with|font-series|medium|<with|font-size|1|<space|0.2fn>.<space|0.2fn>>>>>|<htab|5mm>>
      <no-break><pageref|auto-22>>

      <vspace*|1fn><with|font-series|<quote|bold>|math-font-series|<quote|bold>|6<space|2spc>What
      the toy model tells us> <datoms|<macro|x|<repeat|<arg|x>|<with|font-series|medium|<with|font-size|1|<space|0.2fn>.<space|0.2fn>>>>>|<htab|5mm>>
      <no-break><pageref|auto-23><vspace|0.5fn>

      <with|par-left|<quote|1.5fn>|6.1<space|2spc>Means, maxima and variation
      <datoms|<macro|x|<repeat|<arg|x>|<with|font-series|medium|<with|font-size|1|<space|0.2fn>.<space|0.2fn>>>>>|<htab|5mm>>
      <no-break><pageref|auto-24>>

      <with|par-left|<quote|1.5fn>|6.2<space|2spc>Run-time
      <datoms|<macro|x|<repeat|<arg|x>|<with|font-series|medium|<with|font-size|1|<space|0.2fn>.<space|0.2fn>>>>>|<htab|5mm>>
      <no-break><pageref|auto-25>>

      <with|par-left|<quote|1.5fn>|6.3<space|2spc>Symmetry and asymmetry
      <datoms|<macro|x|<repeat|<arg|x>|<with|font-series|medium|<with|font-size|1|<space|0.2fn>.<space|0.2fn>>>>>|<htab|5mm>>
      <no-break><pageref|auto-26>>

      <vspace*|1fn><with|font-series|<quote|bold>|math-font-series|<quote|bold>|7<space|2spc>Discussion>
      <datoms|<macro|x|<repeat|<arg|x>|<with|font-series|medium|<with|font-size|1|<space|0.2fn>.<space|0.2fn>>>>>|<htab|5mm>>
      <no-break><pageref|auto-27><vspace|0.5fn>

      <with|par-left|<quote|1.5fn>|7.1<space|2spc>Formal notation
      <datoms|<macro|x|<repeat|<arg|x>|<with|font-series|medium|<with|font-size|1|<space|0.2fn>.<space|0.2fn>>>>>|<htab|5mm>>
      <no-break><pageref|auto-28>>

      <vspace*|1fn><with|font-series|<quote|bold>|math-font-series|<quote|bold>|8<space|2spc>Conclusion>
      <datoms|<macro|x|<repeat|<arg|x>|<with|font-series|medium|<with|font-size|1|<space|0.2fn>.<space|0.2fn>>>>>|<htab|5mm>>
      <no-break><pageref|auto-29><vspace|0.5fn>
    </associate>
  </collection>
</auxiliary>