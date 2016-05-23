<TeXmacs|1.0.7.2>

<style|article>

<\body>
  <doc-data|<doc-title|Increasing model efficiency by dynamically changing
  model representations>|||<\doc-author-data|<author-name|Randall
  Gray>|<\author-address>
    CSIRO Division of Marine and Atmospheric Research
  </author-address>>
    \;
  </doc-author-data>|<doc-author-data|<author-name|Simon
  Wotherspoon>|<\author-address>
    University of Tasmania
  </author-address>>>

  <\abstract>
    <\verse>
      <em|We use one model,>

      <em|but many may be more deft.>

      <em|Is it worth trying?>
    </verse>

    \;

    There are a number of strategies to dealing with modelling large complex
    systems -- in our case, large marine ecosystems. These systems are often
    comprised of many submodels, each representing a particular process or
    participant in a way which tries to capture the dynamics which contribute
    to the overall trajectory of the system. The balance between the
    acceptible modelling error and the run-time often dictates the form of
    submodels. There may be scope to improve the position of this balance
    point in both regards by structuring models so that submodels may change
    their algorithmic representation and state space in response to their
    local state and the state of the model as a whole.

    This paper uses an example system consisting of a single population of
    animals which periodically encounter a diffuse contaminant in a localised
    region. We compare the performance of a population-based representation,
    an individual-based representation, and a <em|mutating model> which
    allows the representation to change from population-based to
    individual-based and vice versa depending on the likelihood of
    contaminant contact. The resulting run-times and contaminant dynamics
    associated with each of the stategies suggest that such a mutating model
    approach may be an avenue to gain accuracy within time constraints, time
    within accuracy constraints, or both.
  </abstract>

  <section|Introduction>

  In modeling the effects of human interactions with the environment and with
  animal and plant populations, researchers and modellers are often faced
  with the dilemma of choosing an appropriate scale for these interactions.
  Population level effects are likely to be modelled with an analytic
  approach, but an agent-based or individual-based approach may be more
  appropriate if our system exhibits behaviour which isn't compatible with
  mean-field assumptions.\ 

  There is a body of literature stretching back several decades which
  discusses individual-based modelling as an effective approach when
  individual variability is perceived to be an important driver of the
  system's dynamics, notably JABOWA and its derivatives (refi://Botkin72:1,
  refi://Botkin72:2). Since the early 1980s, a rapidly increasing number of
  significant papers and books have appeared which address the use of
  individual-based models across a broad \ and with discussion of the
  relative strengths and weaknesses of the approach (such as
  refi://Huston88:1, refi://DeAngelis92:1 and refi://Grimm05:1). The use of
  classical (equation based) models to explore populations and ecological
  systems goes back much further, arguably to the end of the eighteenth
  century with Malthus's <em|<strong|An Essay on the Principle of
  Population>> (refy://Malthus:1). \ 

  In some sense these approaches represent extrema in a spectrum which
  exhibits, through its range, varying degrees of aggregation and resolution
  in time, space, and membership. Models at the boundaries of this spectrum
  are obvious representatives of a ``choice of paradigm,'' but there are a
  large number of models which incorporate representatives from the extrema,
  and indeed adopt intermediate representations, such as described by
  refi://Scheffer95:1. Each of these representations has advantages: perhaps
  in the mathematical power of the technique, the speed of simulation or the
  sensitivity to the life-history of individuals. Many systems operate in a
  number of different ``modes'' which may be associated with distinct spatial
  or temporal scales. \ Examples of this include the difference in a plant or
  animal's diurnal activity, seasonal changes such as hibernation, and the
  very dramatic difference in the scales associated with grazing and with
  flight from predators. Perhaps a better approach is to \ dynamically adopt
  a model representation appropriate to the ``mode'' of the system and make
  use of the strengths of the alternative approaches.

  As ecosystem models become broader in scope, including more species and
  richer environments, the notion that a single model can be identified
  within some particular region of this spectrum addresses all members and
  processes equally well starts to break down. Simulation models often embed
  the subject of study within an ``environment'' comprised of primary data
  and other interacting models within the system. The components of the
  environment may simultaneously lie in quite a number of places on the
  spectrum of representations. The actual implementation may be anything from
  a set of quite distinct submodels which are coupled together but retain
  their independence and, in some sense, stand as models in their own right,
  to a corpus of code where the submodels are integrated to the point where
  there is no real distinction between one ``model'' and the next.

  As the climate changes and the environment we live in and use drifts
  further from its familiar state, there is a corresponding need to manage
  human interaction with ecosystems more carefully. The scope of ecosystem
  models is steadily increasing (refi://DeAngelis98:1, refi://Harvey03:1
  refi://Fulton04:1, refi://Gray06:1). Models are including more functional
  groups within their modeled ecosystems and the interactions between
  components are becoming more detailed. Addressing the increased demand for
  detail is costly in terms of computational load: individual-based models of
  populations may be very good at capturing the vulnerability to exceptional
  events, but such simulations take a long time. Worse still, much of this
  time may be spent with the model in a largely unchallenging or
  uninteresting part of its state-space.

  As a simple example, we might consider the motion of a mass capable of
  acceleration; perhaps a rocket or a taxi. The rules of Newtonian motion
  make a good model as long as the mass doesn't move too quickly, and, while
  relativistic motion is much more accurate at high speeds, it is more
  expensive to calculate. A simulation of a spacecraft (or taxi) that
  approaches the speed of light would spend a lot of time calculating
  relativistic motion when a simpler model would be adequate. It is easy to
  imagine that we might use Newton's equations when the velocities are low
  (most of the state space), but shift to relativistic motion when we stray
  from the areas where Newton is comfortably accurate.

  The notion we explore in this paper is that we might change the
  <em|representation> of a submodel based on its location in its state-space,
  and that by doing so we may actually be able to simulate the system more
  effectively. To some degree modellers do this anyway: time-steps or spatial
  resolutions are changed, particular code paths may be by-passed according
  to local conditions within the simulation, or additional calculations might
  be performed to reduce the error when the state is changing rapidly; but
  these optimisations are largely optimisations of the <em|encoding> of the
  model or submodels, rather than an actual change in representation. This
  sort of optimisation increases the model's complexity. In contrast, moving
  from a population modelled by a set of differential equations to a model
  tracking the life-history of a large number of individuals involves a
  change of domain, state-space and changed assumptions concerning the
  homogeneity of the consituent members of the population being modelled, but
  neither the population-based nor the individual-based models are made more
  complex. There must be additional code to map the state of each
  representation to the domain of the other, but these mapping functions are
  discrete and not really part of the models at all.

  This approach to the problem of managing complex simulations was developed
  in light of experiences with several large scale human-ecosystem
  interaction models (refi://Lyne94:1, refi://Gray06:1, and modelling similar
  interactions in a larger study of the region containing the Ningaloo Marine
  Park (<em|work in progress>)). In each of these studies a significant
  component of the model focused on simulate the interaction between
  organisms and contaminant plumes. The focus in Lyne <em|et al.> was the
  potential for the percolation of contaminants originating in industrial
  waste up through the food chain into commercially exploited fish stocks.
  Gray <em|et al.> developed a regional model focussed on assessing ways of
  managing the impact of human activity on the biological systems along the
  Northwest Shelf of Australia, one component of which was the effects on
  commercial prawn fisheries of flushing bitterns from salt production into
  the surrounding environment.

  refi://Monte09:1 presents a lucid discussion of <em|contaminant
  migration-population effects> models. These models incorporate the movement
  of populations and their internal distribution, the transport of
  contaminants through the system via biotic and abiotic pathways, and the
  changes in behaviour and population dynamics associated with contamination.
  Monte discusses \ a method of coupling the equations which govern
  contaminant dispersion with the equations for population dynamics and
  migration. The technique depends on the equations of the location and the
  dispersion of members of a population satisfying an independence condition
  with respect to time and location which must hold. He summarises the
  implications of the conditions, stating that the class of systems where the
  ``movement of animals, the death and birthrates of individuals in
  <math|<with|math-font-series|bold|x>> [location] at instant <math|t> [time]
  depend on previously occupied positions'' is not generally amenable to the
  approach, and suggests that repeated simulations of many individuals is an
  appropriate way of dealing with this situation.

  The treatment of contaminants is expensive in terms of run-time and memory
  use. The model described by refi://Gray06:1 contaminant modelling increased
  the time taken by roughly an order of magnitude, and in all of these
  studies a considerable amount of time was spent in regions where no
  interaction with contaminant plumes was possible.

  Running a complex model and maintaining its state in a region where a
  simple model may perform better, or just as well, imposes a burden which is
  unnecessary. There is potential for significant improvements by reducing
  run-time and modelling error if representations are switched appropriately.
  In doing this, there are four basic questions that arise: ``When should a
  model change representation?'', ``What data needs to persist across
  representations?'', ``How is the initial state for a new representation
  constructed?'' and ``How should the error associated with the loss of state
  information be managed?'' In general, the answer to these questions is
  specific to the ensemble of submodels in question. Before expending the
  resources and effort required to implement such a strategy in a large scale
  model there needs to be, <em|prima facie>, a demonstration that the notion
  is worth pursuing. The aim of this paper is to provide this demonstration
  rather than to develop a body of techniques supporting the approach.

  We present a model of organisms moving along a simple migratory path which
  intersects a region with a field of contamination with varying levels. This
  model exhibits fundamental attributes of larger studies of
  pollutant/ecosystem interactions (refi://Lyne94:1 and refi://Gray06:1), and
  while it isn't intended to accurately represent any particular system, it
  might loosely correspond, for example, to some body of water influenced by
  contaminant loads associated with terrestrial runoff resulting from intense
  rainfalls. Its role is to help explore some of the issues associated with
  changing representations and to test the hypothesis that we might gain
  efficiency in simulation time, representational accuracy, or both.

  Our strategy uses two basic representations of the group of organisms; the
  first uses a single submodel to represent the whole group in a simple
  population-based submodel, the second uses an instance of an
  individual-based submodel to represent each organism in the group. The
  mutating submodel is derived from these two basic representations, and it
  switches between them when appropriate. The population model requires the
  assumption that the likelihood of an individual being at any given location
  within its ambit is represented by its distribution function and that the
  population's contact with the contaminant can be distributed through its
  members according to this function.

  <section|The models>

  The test models are composed of one or more submodels which run within a
  simple time-sharing system. Each submodel runs for a nominated period of
  time and passes control to the next submodel, very much like tasks running
  in many modern computer operating systems.

  The population-based and individual-based submodels have been kept as
  similar as practicable in order to minimise the sources of divergence. Both
  representations are used to model a group of organisms which proceed around
  a common circular migratory path, <math|<with|math-font-series|bold|m>(t)>,
  on an annual basis. While both submodels use the same uptake-depuration
  equation to calculate their contaminant load, the calculation of the
  contact with the contaminant differs since the interactions of an
  individual moving through a plume is, in some sense, an integration across
  a path, while the population's expected interactions are more analogous to
  the integration over the domain of the population.

  The individual-based representation models the group as a set of
  individuals (agents, or instances of submodels) which move in a directed
  random walk. At each time step the trajectory of each individual
  incorporates a directional component toward the ``target location'' on the
  migratory path which corresponds to the notional ideal centre of the group
  at that time. Each agent maintains its own contaminant load, position and
  velocity, and these attributes are independent of the state of the other
  agents.

  Our population-based approach models the organisms with a radially
  symmetric distribution whose centroid lies on the migratory path. In the
  individual-based test model, the density of the individuals around their
  target location arising from their movement model closely follows the
  probability density function of a 2d normal distribution. From this
  representation of the individuals' density, we define the population's
  density, <with|mode|math|\<rho\>(<with|math-font-series|bold|p>)>, to be
  the value of the standard 2d normal distribution with a mean of zero in
  each ordinate and a variance. So our population's density at
  <math|<with|math-font-series|bold|p>> is given by

  <\equation*>
    <math|\<rho\>(\|<with|math-font-series|bold|p>\|) = S<rsub|<rsub|L>
    ><rsup|><frac|1|2\<pi\>\<sigma\><rsup|2>>
    exp<left|(>-<frac|p<rsub|x><rsup|2> +
    p<rsub|y><rsup|2>|2\<sigma\><rsup|2>><right|)>>
  </equation*>

  \;

  where <math|<with|math-font-series|bold|p> =(p<rsub|x>, p<rsub|y>)> is the
  position in the population disk relative to its centre, <math|S<rsub|L>> is
  a scaling parameter (equal to 1.015) which ensures that the population
  density, <math|\<rho\>>, integrated over the finite disk with a radius,
  <math|\<tau\>>, is one, and <math|\<sigma\><rsup|2>> is the variance for
  the distribution. The effective radius of the population, <math|\<tau\>>,
  was taken to be three times the variance in the distance of the individuals
  from <math|<with|math-font-series|bold|m>(t)>. If the distribution of
  individuals around the point <math|<with|math-font-series|bold|m>(t)> and
  the distribution of the population around
  <math|<with|math-font-series|bold|m>(t) >were not consistent, then the
  contaminant exposure of the two submodels would be incompatible and the
  sort of mutation error discussed in the introduction becomes significant.

  In contrast to individuals, populations do not need to maintain position or
  velocity since both are determined by <math|<with|math-font-series|bold|m>(t)>.
  They do, however, maintain the number of organisms they represent and, in
  order to make mutation simpler, populations maintain a list of
  contaminants. In the homogeneous population model the length of this list
  never exceeds one, since the population has only one mean, but in a
  mutating submodel, the contaminant loads of the individuals are carried
  along and decayed as described in section <reference|Circle-and-plume>.

  <subsection|The migratory circle and the plume><label|Circle-and-plume>

  The migratory path is the circular periodic function <math|\<b-m\>(t)>
  which has a radius of <math|R> and a period <math|Y>. This path is used for
  all the variant submodels of the system.

  The plume is modelled as a circular or elliptical cloud of some contaminant
  which intersects this path and has an intensity which varies sinusoidally
  at a frequency which is relatively prime to the annual migration cycle.

  Its centroid is positioned on the migratory circle. The plume is modelled
  as an intensity at a location with an attenuation function. The intensity
  at a point, <math|\<b-r\>>, within the plume at time <with|mode|math|t> is
  given by

  <\equation*>
    <with|mode|math|I(t,<with|math-font-series|bold|r>) = <frac|1|2>(1 +
    cos(2\<pi\>t/p)) exp(-\<psi\> \<phi\>(<with|math-font-series|bold|r>,<with|math-font-series|bold|m><rsub|plume>))>,
  </equation*>

  \;

  where <with|mode|math|p> is the period of the plume,
  <math|<with|math-font-series|bold|m><rsub|plume>> is the centroid of the
  plume, <math|\<psi\>> is a decay exponent, and for a circular plume the
  distance function <math|\<phi\>> is taken to be
  <math|\<phi\>(<with|math-font-series|bold|a>,<with|math-font-series|bold|b>)
  = \|<with|math-font-series|bold|a>-<with|math-font-series|bold|b>\|>. The
  intensity is made elliptical by instead taking
  <math|\<phi\>(<with|math-font-series|bold|a>,<with|math-font-series|bold|b>)=<sqrt|
  (<with|math-font-series|bold|a>-<with|math-font-series|bold|b>)\<cdot\>
  (<sqrt|2|>,<sqrt|1/2>)><rsup|>> since this transformation preserves the
  area of the plume. The effective radius of the circular plume in the model
  is about 3.7% of <math|R>.

  <subsection|Contaminant load and contact>

  The equation for uptake and depuration of our contaminant is the same for
  both representations. Initially a ``total contact'' is calculated for the
  time step. As might be expected, the total contact for a population is
  calculated in a different way to the total contact of an individual. In
  either case, this resulting contact is fed through a standard
  uptake-depuration equation

  <\equation*>
    d C/d t = u M - \<lambda\>C
  </equation*>

  which is solved numerically with a fourth order Runge-Kutta algorithm for
  the value of <with|mode|math|C> given a contact mass, <math|M>, and an
  initial contaminant value or vector of values for <math|C>.

  For individuals, the mass of contaminant which is available for uptake, or
  contact, is taken to be the result of integrating the intensity of the
  plume over the path of the individual, <with|mode|math|\<b-P\><rsub|t>> to
  <with|mode|math|\<b-P\><rsub|t+\<delta\>>>,

  <\equation*>
    M =<big|int><rsub|\<b-P\><rsub|t><rsub|>><rsup|<with|math-font-series|bold|P><rsub|t+\<delta\><rsub|>>>I(<with|math-font-series|bold|p>)
    * \|\|\<b-p\>\|\| d<with|math-font-series|bold|p>
  </equation*>

  where we assume that the motion between
  <with|mode|math|(t,\<ell\><rsub|t>)> and <with|mode|math|(t +
  \<delta\><rsub|c>, l<rsub|t+\<delta\>>)> is linear. Populations are
  somewhat more complex. Here, we calculate the definite integral

  <\equation*>
    M = <big|int><rsub|\<b-P\><rsub|t><rsub|>><rsup|<with|math-font-series|bold|P><rsub|t+\<delta\>>>2<big|int><rsub|<with|math-font-series|bold|\<Omega\>>>I(<with|math-font-series|bold|p
    +><with|math-font-series|bold|\<omega\>>) *
    \<rho\>(<with|math-font-series|bold|p>
    +<with|math-font-series|bold|\<omega\>>)
    d<with|math-font-series|bold|\<omega\>>
    d<with|math-font-series|bold|p><rsup|<rsub|>>
  </equation*>

  where <with|mode|math|<with|math-font-series|bold|\<Omega\>>> is an area
  over which we assess the effective area of the population and
  <math|><math|\<b-p\>+\<omega\>> denotes the area
  <math|<with|math-font-series|bold|\<Omega\>>> translated so that its
  centroid corresponds to <math|\<b-p\>>. The contact equations are solved
  using a simple adaptive quadrature routine.

  <section|The mutating submodel>

  The mutating submodel is formed by interleaving the population
  representation and the individual-based representation. The transitions
  between a submodel and its alternate representation are dictated by its
  proximity to the contaminant plume. Initially the test model begins with
  the group of organisms represented as a population. The starting location
  is far enough away from the plume that there is no chance of any
  interaction between organisms represented by the population and the
  contaminant field. As the population moves about the circle it reaches a
  point at which there is a likelihood that sometime in the next few time
  steps an individual within its disk could conceivably encounter
  contaminants, and at this point the population is disaggregated into
  submodels representing individuals. Similarly, an individual-based submodel
  switches to a population submodel when there is no possibility that it will
  encounter the plume. These decisions obviously rely on knowledge of the
  system; specifically the static location of the plume, the rates of
  movement and the length of time steps.\ 

  The scenario presented exhibits traits that are common to a number of
  situations which may pertain to broad class of contaminant uptake models.
  Many contaminant sources, such as stormwater outfalls and agricultural
  crops, are relatively stationary and in some regards their ``plumes'' are
  reasonably well bounded and predictable. The migratory path is a simple
  equivalent to some periodic behaviour that brings our simulated animals
  into contact with the area of the plume, such as the seasonal movement from
  one foraging ground to another, migration for breeding purposes, and even
  the long, isolated intervals underground exhibited by cicadas.\ 

  The contaminant levels of the individual-based submodels subsumed by the
  population representation are maintained and updated every step by the
  application of the uptake-depuration equation. \ No relative location
  information is maintained.

  <section|Experimentation>

  The comparison of the three modelling schemes is based on the following:

  <\enumerate-roman>
    <item>the correspondence between the total contaminant loads across the
    biomass through time

    <item>a comparison of variability in contaminant load through time
    amongst the representations

    <item>the run-times for each representation

    <item>and the sensitivity to variability in the nature of the plume.
  </enumerate-roman>

  \;

  Our strategy is to first establish the equivalence of the results from the
  mutating submodel representation and the individual-based representation.
  This component of the study also provides us with data for comparing the
  relative efficiency of the representations in terms of run-time. We
  generate forty trials each simulating forty individuals for twelve years in
  each of the two individual-based representations using a circular
  contaminant field. The homogeneous population model is deterministic and so
  one run is sufficient.

  A second set of trials is used to compare the performance of the mutating
  test model against the homogeneous population model when the contaminant
  plume is elliptical. Ideally, the results with an elliptical plume would be
  consistent across the model representations, though the results will show
  this is not the case.

  The analysis in this section is based on two sets of simulations. The
  purpose of each of these sets is to provide data for a comparison of
  run-times, the equivalence (or lack of equivalence) amongst the submodels,
  and to examine the robustness of the representations to changes in the
  configuration of the plume.

  The first set is comprised of forty trials of each of the homogeneous
  representations (individual-based and population-based) and the mutating
  model. Each of these trials tracked either forty individuals or a
  population which represented forty individuals or a mixed system. In this
  set of runs the contaminant plume was circular and its centroid was on the
  migratory circle. This set of runs was conducted to establish the relative
  cost in cpu-time of the three approaches and to experimentally verify (at
  least to some degree) the equivalence which ought to exist between the
  purely individual-based submodel and the mutating submodel.\ 

  A set of trials with an elliptical plume were run to generate a set of data
  for comparison against the the data from the trials with a circular plume.
  Our analysis in these trials is based on the data from the first series of
  trials and from eighty trials of forty individuals with a mutating
  representation and a corresponding population-based trial with each trial
  covering twelve years. The elliptical plume has a major axis which is
  tangential to the migratory path and covers the same area as the circular
  plume. The integral over the area of the plumes are the same, up to the
  numerical error in the quadrature. Our initial analysis will only consider
  the trials in the first set of runs; a comparison of the reponses to the
  elliptical plume will be treated separately.

  Both population-based and individual-based representations suppress contact
  evaluation when they are sufficiently far from the contaminant source. No
  other significant optimisations of the code have been made, and as far as
  possible the same routines are used in both representations. The motivation
  for this approach is to provide a common baseline which we can use to
  measure the utility of the adaptive representation.

  To ensure that run-time comparisons are meaningful all of the simulations
  in the first set of trials were run on the same computer. Each of the
  configurations simulated forty organisms for twelve years.

  <subsection|Contaminant load correspondence between representations>

  The set of trajectories arising from the various representations aren't
  directly comparable. Our individual-based representation produces a time
  series of contaminant levels for each individual, while the population
  submodel produces a ``mean load'' across the whole group of entities it is
  representing. The mutating submodel sits between the two, sometimes
  producing individual time series and sometimes mean time series for varying
  parts of the population. We denote representations by a subscript <math|r
  \<in\> {i,m,p}>, so that <math|C<rsub|r k j>(t)> is the time series
  associated with individual <math|j> in trial <math|k> of representation
  <math|r>, <math|C<rsub|r k>(t)> is the mean over all the individuals in the
  indicated representation and trial, and <math|C<rsub|r>(t)> denotes the
  mean of <with|mode|math|C<rsub|r k>(t)> across the <math|k> trials for the
  indicated representation. In order to compare the dynamics of the system we
  generate mean time series for each of the <math|k> trials in the
  individual-based and mutating sets, <math|<rsup|>C<rsub|i k>(t)> and
  <math|<rsup|>C<with|math-font-series|bold|><rsub|m k>(t)>, paying special
  attention to generating the correct mean in the mutating submodel from time
  steps which have a mixture of individual trajectories and mean trajectories
  from population-based representations. Each of mean time series,
  <math|C<rsub|r k>(t)>, correspond to the mean contaminant load of the
  population, <math|C<rsub|p>(t)>, produced by the population submodel;
  averaging them -- constructing

  <\equation*>
    \ <math|C<rsub|r>(t) = <frac|1|k><big|sum><rsup|k><rsub|j=1>C<rsub|r
    j>(t)>,
  </equation*>

  where <math|r> is one of `<math|i>' or `<math|m>' -- is equivalent to
  running many stochastic trials and averaging to fit the population
  submodel.

  Using <math|<rsup|>C<rsub|i k>(t)>, <math|<rsup|>C<rsub|m k>(t)> and
  <math|<rsup|>C<rsub|p>(t)> we find the maximum value attained for each
  representation, <math|<wide|C|^><rsub|r>>. We are also interested in the
  mean value across time of each representation,<hspace|\<htab\|0\>>\ 

  <\equation*>
    <math|<wide|C|\<bar\>><rsub|r> = <frac|1|T><big|sum><rsub|t\<in\>T>C<rsub|r
    >(t)>
  </equation*>

  These data are presented in Table <reference|MaximaMeans>.

  <\big-table>
    <tabular*|<tformat|<twith|table-lborder|2>|<twith|table-rborder|2>|<twith|table-tborder|1ln>|<cwith|1|1|1|3|cell-bborder|1ln>|<twith|table-bborder|1ln>|<table|<row|<cell|<math|Series<rsub|r>>>|<cell|<math|<wide|C|^><rsub|r>>>|<cell|<math|<wide|C|\<bar\>><rsub|r>>>>|<row|<cell|<math|C<rsub|i>>>|<cell|<with|mode|math|0.1787>>|<cell|<with|mode|math|0.0390>>>|<row|<cell|<math|C<rsub|m>>>|<cell|<with|mode|math|0.1821>>|<cell|<with|mode|math|0.0392>>>|<row|<cell|<math|C<rsub|p>>>|<cell|<with|mode|math|0.1387>>|<cell|<with|mode|math|0.0350>>>>>>

    \;
  </big-table|Maxima and Means<label|MaximaMeans>>

  These data suggest that the mutating representation is consistent with the
  homogeneous individual-based representation. \ The population-based
  representation seems to present markedly different mean and maximum values.

  \;

  <subsection|Contaminant load variability><label|LoadVar>

  We calculated measures of variability in the time series using the
  aggregated time series <math|<rsup|>C<rsub|i k>(t)> and
  <math|<rsup|>C<rsub|m k>(t)> and their respective means across the <math|k>
  trials, \ <math|C<rsub|i>(t)> and <math|C<rsub|m>(t)>. We'll take <math|T>
  to be the total number of time steps taken, and we take

  <\equation*>
    <math|<wide|\<sigma\>|^><rsub|a b>= max<rsub|t\<in\>[1,T]><left|[><frac|1|k><big|sum><rsub|j
    = 1><rsup|k>(C<rsub|a k>(t) - C<rsub|b>(t))<rsup|2><right|]><rsup|1/2>>
  </equation*>

  and

  <\equation*>
    <with|math-font|Bbb*|<wide|\<sigma\>|\<bar\>>><rsub|a b>
    =<left|[><frac|1|T><big|sum><rsub|t=1><rsup|T><left|[><frac|1|k><big|sum><rsub|j
    = 1><rsup|k>(C<rsub|a k>(t) - C<rsub|b>(t))<rsup|2><right|]><right|]><rsup|1/2>,
  </equation*>

  to be the maximum root mean square error and the average root mean square
  error. Clearly we can write <math|<wide|\<sigma\>|\<bar\>><rsub|r r>> as
  <math|<wide|\<sigma\>|\<bar\>><rsub|r>> without introducing ambiguity, and
  similarly for <math|<wide|\<sigma\>|^><rsub|r>>. The values for these
  measure of of variability are presented in Table <reference|LoadVarTbl>.

  <\big-table>
    <tformat|<cwith|1|-1|1|-1|cell-halign|c>|<tabular*|<tformat|<twith|table-lborder|2>|<twith|table-rborder|2>|<twith|table-tborder|1ln>|<twith|table-bborder|1ln>|<cwith|1|1|1|4|cell-bborder|1ln>|<table|<row|<cell|r.m.s.e.>|<cell|<math|r=i>>|<cell|<math|r=m>>|<cell|<math|r=p>>>|<row|<cell|<math|<wide|\<sigma\><rsub|>|^><rsub|i
    r>>>|<cell|<with|mode|math|0.0083>>|<cell|<with|mode|math|0.0084>>|<cell|0.0534>>|<row|<cell|<math|<wide|\<sigma\>|\<bar\>><rsub|i
    r>>>|<cell|0.0024>|<cell|<with|mode|math|0.0024>>|<cell|<with|mode|math|0.0096>>>|<row|<cell|<math|<wide|\<sigma\><rsub|>|^><rsub|m
    r>>>|<cell|<with|mode|math|0.0090>>|<cell|<with|mode|math|0.0090>>|<cell|0.0538>>|<row|<cell|<math|<wide|\<sigma\>|\<bar\>><rsub|m
    r>>>|<cell|0.0024>|<cell|<with|mode|math|0.0024>>|<cell|<with|mode|math|0.0096>>>>>>><hspace|\<htab\|0\>><hspace|\\hfill>
  <|big-table>
    Deviations amongst the model runs with respect to a given
    mean<label|LoadVarTbl>
  </big-table>

  The data here indicate that the variability about the mean is consistent in
  the two representations which use simulated individuals as an avenue for
  estimating contact and uptake. \ This is what we would expect since the
  mechanisms of uptake and contact are the same. \ In contrast, the
  population's values suggest that the contact and uptake are quite
  different, and that the model does not perform in quite the same way.

  <subsection|Sensitivity to the shape of the plume>

  We will use the same notation as Section <reference|LoadVar> for the data
  derived from the circular plumes, while we will add a prime symbol to the
  data derived from the elliptical plumes. Thus, the mean value time series
  for the mutating submodel with elliptical plumes would be denoted
  <math|C<rprime|'><rsub|m>> and the mean value of that time series is
  <math|<wide|C|\<bar\>><rprime|'>>.

  There is a good correspondence between the means and deviations associated
  with the mutating model in the circular and elliptical plume scenarios, but
  there is much poorer correpsondence inthe population based results in the
  two scenarios. The data for the circular plume and for the elliptical plume
  are presented in Tables \ <reference|Symplume> and <reference|Asymplume>
  respectively.

  <big-table|<tabular*|<tformat|<twith|table-tborder|1ln>|<cwith|1|1|1|7|cell-bborder|1ln>|<twith|table-bborder|1ln>|<table|<row|<cell|<math|Series<rsub|r>>>|<cell|<math|<wide|C|^><rsub|r>>>|<cell|<math|<wide|C|\<bar\>><rsub|r>>>|<cell|>|<cell|StdDev>|<cell|<math|r=m>>|<cell|<math|r=p>>>|<row|<cell|<math|C<rsub|m>>>|<cell|<with|mode|math|0.1738>>|<cell|<with|mode|math|0.0392>>|<cell|>|<cell|<math|<wide|\<sigma\><rsub|>|^><rsub|m
  r>>>|<cell|<with|mode|math|0.0088>>|<cell|0.0535>>|<row|<cell|<math|C<rsub|p>>>|<cell|<with|mode|math|0.1387>>|<cell|<with|mode|math|0.0350>>|<cell|>|<cell|<math|<wide|\<sigma\>|\<bar\>><rsub|m
  r>>>|<cell|<with|mode|math|0.0024>>|<cell|<with|mode|math|0.0098>>>>>>|Circular
  plume results<label|Symplume>>

  <big-table|<tabular*|<tformat|<twith|table-tborder|1ln>|<twith|table-bborder|1ln>|<cwith|1|1|1|7|cell-bborder|1ln>|<table|<row|<cell|<math|Series<rsub|r>>>|<cell|<math|<wide|C<rprime|'>|^><rsub|r>>>|<cell|<math|<wide|C<rprime|'>|\<bar\>><rsub|r>>>|<cell|>|<cell|StdDev>|<cell|<math|r=m>>|<cell|<math|r=p>>>|<row|<cell|<with|mode|math|C<rprime|'><rsub|m>>>|<cell|<with|mode|math|0.1856>>|<cell|<with|mode|math|0.0394>>|<cell|>|<cell|<math|<wide|\<sigma\><rsub|>|^><rsub|m
  r><rprime|'>>>|<cell|<with|mode|math|0.0087>>|<cell|0.0616>>|<row|<cell|<math|C<rprime|'><rsub|p>>>|<cell|<with|mode|math|0.1763>>|<cell|<with|mode|math|0.0445>>|<cell|>|<cell|<math|<wide|\<sigma\>|\<bar\>><rsub|m
  r><rprime|'>>>|<cell|<with|mode|math|0.0025>>|<cell|<with|mode|math|0.0092>>>>>>|Elliptical
  plume results<label|Asymplume>>

  The population based model is clearly more sensitive to the shape of the
  plume than the mutating model. It seems likely that the major driver of
  this difference is that the long axis of the plume (a region where the net
  contact will be higher) remains in close proximity to the centroid of
  population where the population density is greatest.

  <subsection|Run-time>

  Each run collected data regarding the amount of time spent in different
  parts of the submodel. As a basis of comparison, the overall amount of time
  spent running on the cpu (linux/unix ``cpu seconds'') for the whole run is
  the most important figure, though the time spent in other parts provides
  illumination into just where the effort is concentrated. Predictably, most
  of the effort is in calculating contact and updating contaminant loads.

  The optimisation of supressing the contact calculations when a population
  is outside the area of potential contact seemed to make very little
  difference to the run-time of population submodel (of the order of 3%), and
  seems unlikely to make a great deal of difference to the mutating submodel.
  In the case of the purely individual-based submodel, this sort of
  optimisation is likely to play a much bigger role in that any penalty would
  be multiplied by the number of animals simulated.

  The population submodel ran for 98.7 cpu seconds. This submodel is
  deterministic and the amount of cpu time used is very stable, so only a
  single run is considered for comparison. The purely individual-based
  submodels took just over a mean time of 4205 cpu seconds with a standard
  deviation of approximately 16 seconds and the mean of the mutating
  submodel's run time was 1157 cpu seconds with a standard deviation of
  slightly over 11 cpu seconds.

  \;

  <section|Discussion>

  The data establish the relative speeds of the three approaches and support
  the assertion that the mutating submodel possesses the same essential
  dynamics as the purely individual-based representation. The essential
  equivalence of the individual-based and mutating representations is well
  supported and there is a significant benefit in run-time with the mutating
  submodel when compared to the individual-based submodel. The population
  submodel, while fastest by several orders of magnitude, showed responses to
  the two distinct contaminant plumes which were significantly different to
  those of the other two submodels.

  The divergence between the population's density function and the observed
  distribution of individuals in the individual-based representation is
  potentially a source of model mismatch or bias. Though the actual
  distribution of individuals is slightly skewed by their motion along the
  migratory circle, it was very close to a two dimensional normal
  distribution and a standard 2d normal distribution was chosen to represent
  the density function of population with its variance taken from the
  observed individuals. Such a distribution corresponds to an ideal situation
  -- in practice there may be a number of environmental influences which make
  a such a simple population distribution unrealistic: in the case of a
  coastal population, exceptions might include mud-flats, sand bars or
  islands.

  \;

  <subsection|Mutating models>

  The mutating model tests whether it may be more efficient in terms of time
  or representational accuracy to change the representation of components of
  the model in response to the local states of its submodels. The specific
  example we are dealing with is very simple, but it possesses many of the
  properties which the general problem of an <em|ad hoc> animal/plume
  interaction might be expected to exhibit, namely the tests for coincidence,
  generating a random walks, and evaluating the uptake and decay of
  contaminant loads. Over a long period of infrequent contact, we would
  expect quite a lot of time to be spent generating random walks and
  evaluating the contact and uptake in places where there is no likelihood of
  contact between animals and the plume. This strategy of changing
  representation is fundamentally about managing the often competing demands
  of representational accuracy, mathematical tractibility, and run-time.

  The mutating submodel is an ensemble of cooperating submodels working
  together like a team in a relay race. Each of the representations has its
  own strengths and passes the baton to another at an appropriate time. So
  each of the submodels must have mappings between other representations
  which preserve as much information as possible without imposing excessive
  burden. As a first step in construction we must determine what information
  should be maintained in each of the representations to ensure that the
  migration from one formulation to another doesn't introduce unreasonable
  error and how that information should be maintained. In our example, the
  <em|significant> information which the individual-based representation
  possesses (which the population representation does not) is the variation
  in contaminants amongst the individuals within the group. We assume that
  the role of the individual locations of these individuals in this model is
  not important over a large portion of the state-space since the interval
  between conversions from individuals to aggregates is large enough to
  ensure a thorough randomisation of their relative position.

  The state spaces of our base-line representations differ, in contrast to
  the example of Newtonian and Relativistic motion. The natural state space
  of the individual based submodel must incorporate position information
  lacking in the population's state space, since the population's movement is
  determined wholly by <math|\<b-m\>(t)> and the individuals each have their
  own path. This is quite different to adaptive models where scales or step
  sizes are adjusted to improve the efficiency of the algorithm. \ Not only
  does the algorithm change in this model, but the domain and range of the
  system changes as well, and the mapping between these state spaces should
  be considered carefully.

  The initial premise was that an individual's trajectory through its
  environment plays a significant role in its contact with contaminants, and
  that the population model doesn't capture this contamination load as well.
  The implication is that the contaminant loads of the individuals must be
  preserved through model transitions, so in this regard the state space
  should remain the same, though our implementation of the uptake and decay
  of the contaminant load of populations has to operate on a list of
  contaminants.

  The transition from individuals to population and population to individuals
  involves the loss and reconstruction of this fine-scale position data. The
  assumption that we can reconstruct a plausible position for each individual
  from the population's distribution function is tenable because there is no
  behavioural change associated with contaminant load -- a contaminant that
  made an organism sluggish would obviously skew the distributions of both
  the population as a whole and the distribution of intoxicated organisms
  within the population. This is an important point: if the data we glean
  from the individual-based representations is valuable, we don't want to
  lose it or corrupt it by an inappropriate treatement of the data. Any
  systematic behaviour arising from the interaction with the plume must be
  accounted for when we transfer from one representation to another.\ 

  In the presented model, individuals are mapped into a population by adding
  their contaminant level to the contaminant list of an appropriate
  population after their movement and contaminant decay have been calculated.
  When a population submodel is mapped back to an individual-based
  representation, new individual-based submodels are created with appropriate
  contaminant levels \ and random locations which are chosen to be consistent
  with the distribution for the population around its centroid. This mapping
  preserves the contaminant load present in the system and the likelihood of
  an encounter between a contaminated individual and new contaminants. In
  this way the distinct contamination levels within the system are maintained
  with a minimum of cost.

  Some overhead is incurred in managing the swap from one representation to
  another. The actual conversion is done by a routine which is integrated
  with the scheduler which manages the transition from one time-step to the
  next. Each of the submodels is able to indicate to the scheduler that a
  change in representation may be reasonable when it determines that it has
  left its domain of efficiency.

  This illustrates the two basic ways of managing the transitions between
  state spaces: by adjusting the algorithmic representation of a submodel to
  maintain essential data, or by dynamically generating missing data at the
  point of transition. \ In our example model both of these mechanisms are
  relatively straightforward, but it is easy to see greater complexity may
  arise. The state information which may be required by an alternative
  representation may not be easy to synthesise, as may be the case where
  there are strong interactions between individuals; or alternatively the
  update step may require a much larger contextual environment.

  <subsection|Transition heuristics>

  When do we swap from one representation to another? More fundamentally,
  <em|how> do we decide to swap? The contraints we impose on this example
  model are quite stringent so the heuristics we can use to decide when to
  change representation may assume a great deal more than might be the case
  in a general situation: we know where the contaminants are at all times,
  the movement of the populations and the individuals are controlled, and the
  behaviour of the simulated organisms does not change through the run. In
  the general case, the heuristics might be much more complex and require a
  great deal more data. Deciding when to change the representation is central
  to the optimisation of the system: changing at the wrong time might
  sacrifice accuracy, efficiency or both.

  In our case, the principal issue is whether or not an agent
  (population-based or individual-based) may encounter contaminants: in our
  case it is simple, there is a single source and the mean movement toward or
  away from that source is quite predictable. As a population enters the
  region of contaminants we can immediately determine that it will
  consistently encounter contaminants for some non-trivial period, and a
  mapping is called for.

  The test which determines if an individual is mapped into a population is
  similarly straightforward: an individual is mapped into a population if it
  is far enough outside the region of contaminants that there is no chance
  that it might encounter them in the next few time-steps, and it is close
  enough to a population. If there is no suitable population, a new one is
  created in order to accomodate it. This spatial constraint is stringent
  enough, when combined with the movement constraints, that the system hasn't
  needed to create more than one population in any of the annual cycles in
  the trials.

  The transition rules in this submodel, from population to individual or
  from individual to population, can be based solely on the distance to the
  contaminant source, the time-step and the speed at which the simulated
  group of organisms moves around its annual path. Our heuristic could be
  generalised to systems where there were multiple, dynamically instanciated
  sources, when the sources of contamination could be determined at the
  beginning of a time-step. This particular submodel attempts to present a
  greatly simplified version of the general case -- if the hypothesis were to
  fail in this case it would be reasonable to assume that it would fail in
  most cases.

  <subsection|Means, maxima and variation>

  In terms of the formulation of the mutating submodel, it is difficult to
  see how there could be a significant difference between its contaminant
  loads and those of the the purely individual-based submodel. The means,
  maxima and the various calculations of deviation between the
  individual-based and mutating submodels were very close and this supports
  the belief that they can be treated as essentially equivalent models of the
  contaminant load in the group of simulated organisms. Both of these
  representations, however, differed noticably from the population submodel;
  the population's mean value, <math|<wide|C|\<bar\>><rsub|p>>, was about 10%
  lower than that of the either <math|<wide|C|\<bar\>><rsub|i>> or
  <math|<wide|C|\<bar\>><rsub|m>>, and the statistics on the variation
  between representations (<math|<wide|\<sigma\>|\<bar\>><rsub|a b>>, and
  <math|<wide|\<sigma\>|^><rsub|a b>>) indicate that there is very little
  difference between the data produced by either the base-line
  individual-based model and the data produced by a mutating model. \ The
  population model, in contrast, produces means and maximum values which are
  significantly lower (11% and 22%) than those of the \ individual-based
  model.

  The discrepancy between the population-based uptake and individual-based
  uptake is enough to suggest the following three possibilities:

  <\itemize-dot>
    <item>the (normal) distribution of the population around its centroid was
    not consistent with the individual's spatial dynamics,

    <item>the extent of the population submodel was not adequately tuned with
    respect to the individuals,

    <item>interactions between the plume and the population or the plume and
    the individuals was not modelled appropriately
  </itemize-dot>

  This is the sort of situation discussed in section <reference|Plume1>. The
  fundamental submodels exhibit different dynamics over the domain of the
  simulation, and this is one of the niches which a mutating approach might
  be most advantageous. It is difficult to accurately describe a wild
  population's distribution across the whole of its domain -- changes in the
  behaviour of individuals (arising from predation, drought or other
  stressors) may engender quite different population distributions, so even
  in the case of a purely population-based representation an argument can be
  made for changing the underlying representation based on the local state.

  <\subsection>
    Representational sensitivity to plume distribution<label|Plume1>
  </subsection>

  The issue of whether the configuration of the plume has an effect on the
  results of a simulation depending on the representation of the organisms is
  important. It is easy to imagine calibrating a population-based
  representation with data from fine-scale individual simulations in order to
  make the most of the economies a population-based representation affords,
  but this may not be a good idea if there is poor correspondence between the
  two submodels over their domain.

  The data from the mutating model in the set of trials with elliptical
  plumes had means, maxima and standard deviations which were consistent with
  the set of trials which used circular contaminant fields. This matches our
  expectations since the total areas covered and the total contaminant load
  over the area are the same for the two representations, and the speed of
  the agents through the plume is relatively slow. In contrast, the circular
  and elliptical trials of the population submodel had markedly different
  maxima and mean values through time.

  The data suggest that the individual-based representation is reasonably
  robust with respect to the plume shape (at least in the context of this
  experiment). The population model did not behave as expected and it may be
  that the model of the distribution of individuals which formed the basis of
  the population submodel may have systematic problems with the population
  wide sampling of the plume -- the increase in mean and maximum levels when
  the plume was elliptical and oriented tangentially along the track of the
  population suggests that the centre of the population is oversampled. The
  implications of this are that we must choose our population distribution
  carefully, and a simple normal distribution about a centroid may not behave
  well with respect to plume dynamics.

  Set against this, the numeric modelling of populations is well established.
  It is quite likely that the individual-based submodel would have failed to
  adequately capture recruitment and mortality dynamics if they had been
  included in the study. In such a situation, the mutating submodel would
  have performed best overall -- largely maintaining the fidelity of each of
  the representations across the state-space.

  \;

  <subsection|Run-time>

  The population submodel is clearly much faster than either of the other two
  representations, and for situations where we can reasonably make mean-field
  assumptions and our distribution model fits well with the observed data it
  is likely that the economies afforded by the speed of execution and the
  well established mathematical understanding of such models will vastly
  outweigh benefits of individual-based representations. In situations where
  mean-field assumptions occasionally fail to hold it is similarly clear that
  there are benefits to using mutating submodels. Purely individual-based
  submodels entail significant overhead, which in our context contributes
  nothing to the outcome. In a richer environment with diverse responses to
  the local state, an individual-based submodel's `non-productive' overhead
  may be competitive with the overheads associated with the management and
  transition of mutating submodels.\ 

  It is difficult to decide what an optimal representation of a system is --
  not only is the trade-off between run-time and resolution a dilemma, but we
  then have to consider (at least in the case of stochastic models) whether
  to go for minimal error, or to maximise the number of trials in the time at
  hand. In the case of error versus number of trials, we can make an informed
  decision on where that balance lies if we have a good estimate of how
  closely the model tracks the ``truth'' but this is often not possible,
  particularly when the subject of the study is poorly observed.

  <section|Conclusion>

  The case for considering modelling schemes where the representation of a
  system changes in response to its local state seems to have merit. There
  are clear advantages in run-time relative to homogeneous individual-based
  simulations. \ In contrast there would seem to be little to gain in
  run-time with a shift from population-based representations to
  individual-based representations, but in situations where there are
  behavioural changes -- particularly changes which affect the distribution
  of individuals -- the scope for increasing the accuracy of the models is
  attractive.

  The principle source of extra overhead in this modelling structure is in
  the scheduler which manages the multiple submodels. \ In the homogeneous
  population model, there is only one submodel running, and the extra cost of
  the more general scheduler over a simple iterative loop is negligible.
  Similarly the overhead in the homogeneous individual-based model is
  comparable to an additional iterative loop over the agents. Neither
  contribute a significant amount to the total time spent running a trial;
  both the solution of the uptake-depuration equation and the calculation of
  the contact dominated the run-time in both homogeneous models.

  This simple model demonstrated that changing the representation of a system
  from one form to another can provide a mechanism for increasing the
  efficiency or accuracy with only a little extra effort. A more challenging
  avenue for study would be a similar model which included population
  recruitment and mortality -- particularly if there were sub-lethal effects
  associated with the contaminant load which changed the behaviour or
  fecundity of the simulated organisms.

  Even if we can count on the regular doubling of computational capacity
  which we've enjoyed for so long, the magnitude of the problems we consider
  seems likely to grow as fast as our capacity and possibly faster. \ In our
  experience of large scale marine ecosystem modelling, the size of the
  system considered is growing much faster than the computational capacity.
  Even for small systems the possibility of adjusting the representation of
  submodels to optimise the accuracy of the model as a whole has great
  appeal. Mutating models may provide an effective means of concentrating the
  use of computational capacity where it is most needed. \ 

  \;

  \;
</body>

<\initial>
  <\collection>
    <associate|info-flag|detailed>
    <associate|page-screen-margin|false>
    <associate|page-show-hf|true>
    <associate|page-type|a4>
    <associate|preamble|false>
    <associate|src-style|angular>
  </collection>
</initial>

<\references>
  <\collection>
    <associate|Asymplume|<tuple|4|7>>
    <associate|Circle-and-plume|<tuple|2.1|?>>
    <associate|LoadVar|<tuple|4.2|6>>
    <associate|LoadVarTbl|<tuple|2|7>>
    <associate|MaximaMeans|<tuple|1|6>>
    <associate|Plume1|<tuple|5.4|10>>
    <associate|Symplume|<tuple|3|7>>
    <associate|auto-1|<tuple|1|1>>
    <associate|auto-10|<tuple|2|6>>
    <associate|auto-11|<tuple|4.3|7>>
    <associate|auto-12|<tuple|3|7>>
    <associate|auto-13|<tuple|4|7>>
    <associate|auto-14|<tuple|4.4|7>>
    <associate|auto-15|<tuple|5|7>>
    <associate|auto-16|<tuple|5.1|7>>
    <associate|auto-17|<tuple|5.2|8>>
    <associate|auto-18|<tuple|5.3|9>>
    <associate|auto-19|<tuple|5.4|9>>
    <associate|auto-2|<tuple|2|3>>
    <associate|auto-20|<tuple|5.5|10>>
    <associate|auto-21|<tuple|6|10>>
    <associate|auto-22|<tuple|6|11>>
    <associate|auto-23|<tuple|6|11>>
    <associate|auto-24|<tuple|6|10>>
    <associate|auto-25|<tuple|7|11>>
    <associate|auto-26|<tuple|6|11>>
    <associate|auto-27|<tuple|6|11>>
    <associate|auto-28|<tuple|7|12>>
    <associate|auto-29|<tuple|8|12>>
    <associate|auto-3|<tuple|2.1|3>>
    <associate|auto-30|<tuple|9|?>>
    <associate|auto-4|<tuple|2.2|4>>
    <associate|auto-5|<tuple|3|4>>
    <associate|auto-6|<tuple|4|5>>
    <associate|auto-7|<tuple|4.1|5>>
    <associate|auto-8|<tuple|1|6>>
    <associate|auto-9|<tuple|4.2|6>>
    <associate|footnote-|<tuple|?|1>>
    <associate|footnote-1|<\tuple>
      1
    </tuple|?>>
    <associate|footnote-2|<\tuple>
      2
    </tuple|?>>
    <associate|footnr-2|<\tuple>
      2
    </tuple|?>>
  </collection>
</references>

<\auxiliary>
  <\collection>
    <\associate|table>
      <tuple|normal|Maxima and Means<label|MaximaMeans>|<pageref|auto-8>>

      <\tuple|normal>
        Deviations amongst the model runs with respect to a given
        mean<label|LoadVarTbl>
      </tuple|<pageref|auto-10>>

      <tuple|normal|Circular plume results<label|Symplume>|<pageref|auto-12>>

      <tuple|normal|Elliptical plume results<label|Asymplume>|<pageref|auto-13>>
    </associate>
    <\associate|toc>
      <vspace*|1fn><with|font-series|<quote|bold>|math-font-series|<quote|bold>|1<space|2spc>Introduction>
      <datoms|<macro|x|<repeat|<arg|x>|<with|font-series|medium|<with|font-size|1|<space|0.2fn>.<space|0.2fn>>>>>|<htab|5mm>>
      <no-break><pageref|auto-1><vspace|0.5fn>

      <vspace*|1fn><with|font-series|<quote|bold>|math-font-series|<quote|bold>|2<space|2spc>The
      models> <datoms|<macro|x|<repeat|<arg|x>|<with|font-series|medium|<with|font-size|1|<space|0.2fn>.<space|0.2fn>>>>>|<htab|5mm>>
      <no-break><pageref|auto-2><vspace|0.5fn>

      <with|par-left|<quote|1.5fn>|2.1<space|2spc>The migratory circle and
      the plume <datoms|<macro|x|<repeat|<arg|x>|<with|font-series|medium|<with|font-size|1|<space|0.2fn>.<space|0.2fn>>>>>|<htab|5mm>>
      <no-break><pageref|auto-3>>

      <with|par-left|<quote|1.5fn>|2.2<space|2spc>Contaminant load and
      contact <datoms|<macro|x|<repeat|<arg|x>|<with|font-series|medium|<with|font-size|1|<space|0.2fn>.<space|0.2fn>>>>>|<htab|5mm>>
      <no-break><pageref|auto-4>>

      <vspace*|1fn><with|font-series|<quote|bold>|math-font-series|<quote|bold>|3<space|2spc>The
      mutating submodel> <datoms|<macro|x|<repeat|<arg|x>|<with|font-series|medium|<with|font-size|1|<space|0.2fn>.<space|0.2fn>>>>>|<htab|5mm>>
      <no-break><pageref|auto-5><vspace|0.5fn>

      <vspace*|1fn><with|font-series|<quote|bold>|math-font-series|<quote|bold>|4<space|2spc>Experimentation>
      <datoms|<macro|x|<repeat|<arg|x>|<with|font-series|medium|<with|font-size|1|<space|0.2fn>.<space|0.2fn>>>>>|<htab|5mm>>
      <no-break><pageref|auto-6><vspace|0.5fn>

      <with|par-left|<quote|1.5fn>|4.1<space|2spc>Contaminant load
      correspondence between representations
      <datoms|<macro|x|<repeat|<arg|x>|<with|font-series|medium|<with|font-size|1|<space|0.2fn>.<space|0.2fn>>>>>|<htab|5mm>>
      <no-break><pageref|auto-7>>

      <with|par-left|<quote|1.5fn>|4.2<space|2spc>Contaminant load
      variability <datoms|<macro|x|<repeat|<arg|x>|<with|font-series|medium|<with|font-size|1|<space|0.2fn>.<space|0.2fn>>>>>|<htab|5mm>>
      <no-break><pageref|auto-9>>

      <with|par-left|<quote|1.5fn>|4.3<space|2spc>Sensitivity to the shape of
      the plume <datoms|<macro|x|<repeat|<arg|x>|<with|font-series|medium|<with|font-size|1|<space|0.2fn>.<space|0.2fn>>>>>|<htab|5mm>>
      <no-break><pageref|auto-11>>

      <with|par-left|<quote|1.5fn>|4.4<space|2spc>Run-time
      <datoms|<macro|x|<repeat|<arg|x>|<with|font-series|medium|<with|font-size|1|<space|0.2fn>.<space|0.2fn>>>>>|<htab|5mm>>
      <no-break><pageref|auto-14>>

      <vspace*|1fn><with|font-series|<quote|bold>|math-font-series|<quote|bold>|5<space|2spc>Discussion>
      <datoms|<macro|x|<repeat|<arg|x>|<with|font-series|medium|<with|font-size|1|<space|0.2fn>.<space|0.2fn>>>>>|<htab|5mm>>
      <no-break><pageref|auto-15><vspace|0.5fn>

      <with|par-left|<quote|1.5fn>|5.1<space|2spc>Mutating models
      <datoms|<macro|x|<repeat|<arg|x>|<with|font-series|medium|<with|font-size|1|<space|0.2fn>.<space|0.2fn>>>>>|<htab|5mm>>
      <no-break><pageref|auto-16>>

      <with|par-left|<quote|1.5fn>|5.2<space|2spc>Transition heuristics
      <datoms|<macro|x|<repeat|<arg|x>|<with|font-series|medium|<with|font-size|1|<space|0.2fn>.<space|0.2fn>>>>>|<htab|5mm>>
      <no-break><pageref|auto-17>>

      <with|par-left|<quote|1.5fn>|5.3<space|2spc>Means, maxima and variation
      <datoms|<macro|x|<repeat|<arg|x>|<with|font-series|medium|<with|font-size|1|<space|0.2fn>.<space|0.2fn>>>>>|<htab|5mm>>
      <no-break><pageref|auto-18>>

      <with|par-left|<quote|1.5fn>|5.4<space|2spc>Representational
      sensitivity to plume distribution<label|Plume1>
      <datoms|<macro|x|<repeat|<arg|x>|<with|font-series|medium|<with|font-size|1|<space|0.2fn>.<space|0.2fn>>>>>|<htab|5mm>>
      <no-break><pageref|auto-19>>

      <with|par-left|<quote|1.5fn>|5.5<space|2spc>Run-time
      <datoms|<macro|x|<repeat|<arg|x>|<with|font-series|medium|<with|font-size|1|<space|0.2fn>.<space|0.2fn>>>>>|<htab|5mm>>
      <no-break><pageref|auto-20>>

      <vspace*|1fn><with|font-series|<quote|bold>|math-font-series|<quote|bold>|6<space|2spc>Conclusion>
      <datoms|<macro|x|<repeat|<arg|x>|<with|font-series|medium|<with|font-size|1|<space|0.2fn>.<space|0.2fn>>>>>|<htab|5mm>>
      <no-break><pageref|auto-21><vspace|0.5fn>
    </associate>
  </collection>
</auxiliary>