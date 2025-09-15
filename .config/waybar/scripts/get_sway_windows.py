#!/usr/bin/env python3

import json
import subprocess

def get_focused_window_title():
    try:
        # Get the Sway window tree in JSON format
        sway_tree_json = subprocess.check_output(['swaymsg', '-t', 'get_tree']).decode('utf-8')
        sway_tree = json.loads(sway_tree_json)

        # Function to recursively find the focused node
        def find_focused_node(node):
            if node.get('focused'):
                return node
            for child in node.get('nodes', []):
                focused_node = find_focused_node(child)
                if focused_node:
                    return focused_node
            for child in node.get('floating_nodes', []):
                focused_node = find_focused_node(child)
                if focused_node:
                    return focused_node
            return None

        focused = find_focused_node(sway_tree)

        if focused and 'name' in focused:
            title = focused['name']
            # Remove common Sway prefixes like H[], V[], T[], S[], L[]	
            # This regex targets the pattern [LETTER][...something...] at the beginning of the string
            # and removes it, including the brackets and content within.
            import re
            clean_title = re.sub(r'^[HVTLS]\[.*?\]\s*', '', title)
            return clean_title
        elif focused and 'app_id' in focused: # Fallback for applications that might not have a 'name' but have an app_id
            return focused['app_id']
        elif focused and 'window_properties' in focused and 'title' in focused['window_properties']:
            return focused['window_properties']['title']
        else:
            # If no focused window or no name/app_id found, return empty
            return ""
    except Exception as e:
        # Log error to stderr if something goes wrong
        print(f"Error: {e}", file=sys.stderr)
        return "Error"

if __name__ == "__main__":
    import sys
    print(get_focused_window_title())
