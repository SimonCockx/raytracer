# TODO

- Make scaling transformations work for lights (if possible) (hangt ook samen met getransformeerde materialen...)
- Hangt samen met vorige fout: maak area lights voor willekeurige shapes -> voeg sample-functie toe aan shapes
- Implement continuous spectrum
- FIX BUG: gamma correction should take place AFTER anti-aliasing in camera -> but which part of the program is responsible of gamma correction??
- Maak Spectrum instance van Num/Floating (-> lost deel van gamma-correctie-probleem op?)
- Return material in findHit! (and remove reflectHit) => reduce double intersection tests in MaxDepthPathTracer
- Momenteel: `inspect` neemt als argument een `Ray`, en returnt de volledige BRDF -> dubbelzinnig?
- Maak achtergrond toch `Ray`-afhankelijk (ipv enkel richting) -> goed voor bv 2.5D-applicaties
- Test path-tracing met achtergrondkleur
- `follow ray t` komt te vaak voor -> voeg toe in `Intersection` OF
  maak nieuw object met `(oppervlaktepunt, normaal(, uvw))`. (is `t` wel nodig? Zorgt ook voor dubbelzinnigheden na transformaties... -> kijk eens goed na of dit een vaste betekenis (eg afstand) heeft)
  Optie 2 is wss beter => laat ook toe om shadowReflect te maken voor een enkele lichtbron => consistenter
- Add light source sampling strategy
- Rename existentials to `Any{type}`
- Lees boek na over color clamping, want nu doe ik het fout.
- Documenteer met referenties naar welk deel van de renderingvergelijking elke functie moet oplossen.
- Rename "spec"-variables to "power"/"emitted radiance"/"incident radiance"/...
