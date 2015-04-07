import os
from pymol import cmd

def reset(options):
    cmd.reset()
    cmd.delete('all')
    cmd.set('bg_rgb', options['background color'])

    if options['light-colored background']:
	cmd.set('depth_cue', 0)
	cmd.set('ray_trace_fog', 0)
