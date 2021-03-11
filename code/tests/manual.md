<!-- Note: edits should be made to the .Rmd version of this file -->

# 1 Overview of the tool

The ActDev tool is a prototype tool to help the planning system better
account for and support sustainable travel. The aim is to demonstrate
what is possible with new datasets and methods and to make the case for
a complete nationally available tool.

Currently the tool only provides data for 35 sites, as outlined in the
[ActDev report](https://actdev.cyipt.bike/report/). This manual
describes the functionality of the tool’s main layers: the landing page
at <https://actdev.cyipt.bike>, the site level web application which is
available at
[actdev.cyipt.bike/sites/](https://actdev.cyipt.bike/sites/) (with
‘sites’ corresponding to the site name), and the A/B Street traffic
simulation tool.

# 2 The landing page

The landing page provides an overview of the sites. In the prototype
version at the time of writing there are 35 sites, although we would
plan to add more in a national tool. The horizontal bars represent the
average number of trips per day that are likely to arise from the site,
with colour representing mode. Changing the scenarios shows how the mode
split could change under different scenarios. Currently only ‘Baseline’
(based on the 2011 Census) and a hypothetical ‘Go Active’ scenarios are
implemented. We would like to refine the scenarios in future work. The
scenarios can also be used in the site level tool, which can be launched
by clicking on a site on the landing page.

# 3 The site level web application

The tool is designed to be used primarily at the site level. The site
level views are available from URLs such as
<https://actdev.cyipt.bike/great-kneighton> and
<https://actdev.cyipt.bike/handforth>.

On the left hand side is the user interface panel. On the right hand
side is the map view, as illustrated in Figure
<a href="#fig:panels">3.1</a>.

<img src="https://user-images.githubusercontent.com/1825120/110693965-f4390600-81df-11eb-8d7a-c37ea0e23f1f.png" alt="The site level view." width="80%" />
<p class="caption">
Figure 3.1: The site level view.
</p>

Key elements in the user interface panel are:

-   The **site name and description** text in the top left provides
    information on number of dwellings from planning application data
    and the average (median) commute distance for people in the
    surrounding area based on the 2011 Census, a reasonable proxy for
    likely commute distances for future residents unless the site comes
    with substantial on-site employment opportunities.
-   The **scenario button** is used to show how travel patterns could
    change. Clicking the toggle changes the travel behaviour graph to
    show how walking and cycling could grow under a scenario of high
    active travel (see report for details).
-   The **planning application link** which, when available, takes you
    to a web page showing the planning application associated with the
    site.
-   The **view simulation** button, which opens up the A/B Street
    traffic simulation link to show travel behaviour at the level of
    individual agents on the network (see the Traffic simulation section
    below for details).
-   **Key site metrics** are shown with colours ranging from good
    (green) to poor (red) levels from an active travel perspective. The
    levels associated with red/amber/red are shown when the user hovers
    over the text, as shown below (see [issue 51 in the actdev-ui
    repo](https://github.com/cyipt/actdev-ui/issues/51) for details).

![](https://user-images.githubusercontent.com/1825120/110697687-9a870a80-81e4-11eb-9e20-9874a7e2a9c4.png)

-   The **mode split graph** provides an overview of travel patterns
    associated with the site. The coloured bars show the average number
    of trips per day by different modes of travel (walking, cycling, car
    driving are shown currently) for different trip distances, based on
    2011 travel to work data reporting travel behaviour in the
    surrounding area.
-   The **site data** buttons shown the core ActDev layers described in
    the next section.
-   **Site photos** provide a visual impression of the site where
    available.

The key elements of the map element to the right of Figure
<a href="#fig:panels">3.1</a> are:

-   The site boundary, shown in light blue
-   The basemap interface in the bottom left, which allows you select
    alternative maps, e.g. the Satellite basemap to provide an
    indication of road widths.
-   Additional layers, which include the following.
    -   Destinations: this layer shows some key destinations such as
        schools, shops and hospitals in close proximity to the site
    -   Buildings: the buildings on the development site which are
        either taken from OpenStreetMap or simulated when OpenStreetMap
        data is unavailable.
    -   Applications: planning applications from the PlanIt API, which
        gives an indication of how many other large planning
        applications for new dwellings and other developments are
        planned in the surrounding area (see the StreetFocus project for
        more detailed site application data)
    -   Collisions: data on where car crashes and other types of vehicle
        collisions have taken place in the surrounding area
    -   Traffic counts: this layer provides an indication of traffic
        levels on some major roads in the map area

## 3.1 Core ActDev Layers

### 3.1.1 Desire lines

![](https://user-images.githubusercontent.com/1825120/110680868-0d868600-81d1-11eb-9342-62461b222b60.png)
The desire lines layer shows the shortest path between origins on the
site and destinations. The layer can be selected by clicking on the
‘Desire lines’ toggle in the left panel.

The purpose of the layer is to highlight the kind of trips people who
live on the site may want to make. Currently the desire lines only
include 2 types of trip:

-   Commuting trips from the 2011 Census
-   Trips to the nearest town

In future work we plan to increase the number of trip types represented
in the desire line and other travel layers. The other travel layers,
which are based on the desire lines, are the Routes and Route network
layers.

### 3.1.2 Routes

The Routes layer shows routes to destinations represented in the desire
lines layer. There are a number of route options shown in the Routes
drop-down menu:

![](https://user-images.githubusercontent.com/1825120/110682147-876b3f00-81d2-11eb-96e9-b86203ec250e.png)
- Walking routes, showing likely walking routes according an online
routing service - Cycling - quiet, showing routes that take off road
paths and avoid busy roads wherever possible - Cycling - balanced,
showing cycling routes from CycleStreets which aim to find a balance
between speed and quietness - Cycling - fast, direct routes from
CycleStreets

### 3.1.3 Route network

The route network shows the same data as the route information but at
the level of individual segments which are typically only a dozen or so
meters in length. The purpose of this layer is to highlight areas on the
network that may have high active travel volumes and places where
interventions may be needed to enable walking and cycling away from busy
roads. This use is well illustrated by comparing the route networks for
Great Kneighton, which has good walking and cycling infrastructure going
to the nearest town vs Handforth, which is surrounded by busy roads, as
illustrated in Figure <a href="#fig:rnet">3.2</a> below.

<img src="https://user-images.githubusercontent.com/1825120/110683382-d9f92b00-81d3-11eb-9ec4-8ac745637762.png" alt="Route network layer for sites Great Kneighton (left) and Handforth (right). Note the predominance of relatively quiet and therefore more cyclable routes in Great Kneighton in blue vs the busy routes that may deter people from walking and particularly cycling on key parts of the route network leading to key destinations in Handforth." width="45%" /><img src="https://user-images.githubusercontent.com/1825120/110683635-27759800-81d4-11eb-83c3-6e5b4ddf224d.png" alt="Route network layer for sites Great Kneighton (left) and Handforth (right). Note the predominance of relatively quiet and therefore more cyclable routes in Great Kneighton in blue vs the busy routes that may deter people from walking and particularly cycling on key parts of the route network leading to key destinations in Handforth." width="45%" />
<p class="caption">
Figure 3.2: Route network layer for sites Great Kneighton (left) and
Handforth (right). Note the predominance of relatively quiet and
therefore more cyclable routes in Great Kneighton in blue vs the busy
routes that may deter people from walking and particularly cycling on
key parts of the route network leading to key destinations in Handforth.
</p>

## 3.2 In site network

The purpose of this layer is to show what the layout of the travel
network is like *in the site boundaries*. This layer depends on the
availability of road data from OSM so is unavailable for sites that have
yet to be built.

## 3.3 Accessibility

This provides an overview of the accessibility of the area directly
surrounding the site, divided up into concentric rings of 1, 3 and 6 km
in radius, with the centrepoint in the centre of the site.

## 3.4 Journey times

This layer presents data from the Department for Transport’s journey
time statistics (JTS) to key destinations.

# 4 Traffic simulation

Clicking on the ‘View simulation’ button in the left panel of the site
level view will open a new tab in your browser showing the site and
surrounding area in the A/B Street traffic simulation software (requires
a modern browser and desktop/laptop computer). The boundaries of this
simulation view are determined by common workplace destinations
associated with the surrounding area and the data in the map is taken
from OpenStreetMap.

Moving around the map in A/B Street works the same as other typical web
maps: click and drag to pan, and zoom with your mouse’s scroll wheel or
touchpad. The view becomes more detailed when you zoom in, showing
individual lanes, intersections, and agents. You can click on these
object to get more information and interact with them.

The simulation starts at 8am, running at 30x real-time speed. The
top-left panel lets you pause, change simulation speed, jump to a
particular time, or rewind to midnight. When you hover over any button,
keyboard shortcuts are shown. Pausing/resuming using the spacebar key is
particularly useful. The color scheme switches automatically at 6am and
6pm to reflect day and night.

The top-right panel lets you switch between the Baseline and Go Active
scenarios. These control the mode split of the people living in the
site. By default, only site residents are simulated, but you can enable
background traffic to simulate people living in the rest of the map and
just outside the map boundaries. This background traffic is synthesized
from 2011 census data.

The top-right panel also has shortcuts for some of the most interesting
tools. You can follow an individual person, which you can also manage by
zooming in and clicking on anybody. You can also open a layer showing
the walking and cycling activity around the map, to determine which
roads are most used by different groups of people. The cycling activity
layer breaks down throughput by roads with some sort of bike-only lane
and not, so you can find areas where bikes may be interacting with cars.
The bottom-right panel has buttons to explore more layers and examine
more data about aggregate trip patterns.

You don’t just passively watch the simulation in A/B Street unfold; you
can modify roads and intersections to try to mitigate some observed
problem. Click edit map in the top-right panel, then zoom in and click
on an individual lane or intersection. You can transform a single lane’s
type, toggling between a general-purpose travel lane, a bike-only lane,
a bus-only lane, street parking (though parking is disabled for the
ActDev simulations), or close the lane for construction. You can also
reverse the direction of a lane, implement traffic calming measures to
control speed, and define low-traffic neighborhoods that don’t allow
through-traffic. Note that changing the width of roads or number of
lanes is not yet supported. Even if a two-way cycletrack would
physically fit in the space that one driving lane takes, you cannot yet
model this type of change. This is future work.

For intersections, you can toggle stop signs to control which road has
right-of-way, and modify traffic signals in great detail, adjusting
timing and changing the movements protected and permitted by each stage
of the signal.

The simulations in A/B Street are deterministic, given the same traffic
scenario and set of map edits. This means that when you make a few edits
to the map, you can precisely compare the effects on individual people
and in aggregate. Once you’re running the simulation with some map
edits, clicking individual people, roads, intersections, and checking
the trip aggregates in the “more data” section will show a comparison
with the baseline of no map edits. Note that mode choice does not change
as a function of your edits; as many people will continue to drive, no
matter how much you improve the bike network. You can use the baseline
and go active scenarios to compare mode shift.

As a final hint, if you are using A/B Street a fair bit and want to
reduce loading times, you can [download and
install](https://a-b-street.github.io/docs/howto/index.html) the
software instead of running it in your web browser. It runs much faster
natively and does not need to constantly download new files. Once you
install it, you need to opt into downloading data for the UK sites
you’re interested in.

# 5 Providing feedback

If you have feedback on the tool please let us know in the [ActDev
questionnaire](https://forms.office.com/Pages/ResponsePage.aspx?id=qO3qvR3IzkWGPlIypTW3yyMypkiIdOJGrvs4vzE0KWxUQzY2WDhJVFNDTzk0Q1oxQlVSSkJaSUVGMi4u)
- this will be quick (around 5 minutes) to fill in and will help us
improve the tool or subsequent tools building on the approach.

If you would like to provide specific technical feedback or provide
reproducible bugs, you can do so (requires a GitHub account):

-   In the [actdev-ui issue
    tracker](https://github.com/cyipt/actdev-ui/issues) if it is a bug
    associated with the website
-   In the [actdev issue
    tracker](https://github.com/cyipt/actdev/issues) if you have a
    specific technical suggestion or would like to report a bug with the
    data

To edit this manual, click
[here](https://github.com/cyipt/actdev/edit/main/code/tests/manual.Rmd)
(requires a GitHub account).
