#!/usr/bin/env python3
"""
Lucian CI/CD v2 - Compatibility Wrapper
=======================================

This script provides backward compatibility with the original Prolog-based
Lucian CI/CD system while using the new modern implementation underneath.
"""

import sys
import subprocess
from pathlib import Path


def run_legacy_command():
    """Run the new system with appropriate parameters to match legacy behavior"""
    
    # Get the directory containing this script
    script_dir = Path(__file__).parent
    new_cicd_script = script_dir / "luciancicd_v2.py"
    
    if not new_cicd_script.exists():
        print("Error: luciancicd_v2.py not found")
        sys.exit(1)
        
    # Run the new CI/CD system
    try:
        result = subprocess.run([
            sys.executable, str(new_cicd_script), "--run"
        ], cwd=script_dir)
        sys.exit(result.returncode)
    except Exception as e:
        print(f"Error running CI/CD: {e}")
        sys.exit(1)


def main():
    """Main entry point for compatibility wrapper"""
    
    # Check if we're being called from SWI-Prolog or command line
    if len(sys.argv) > 1:
        command = sys.argv[1].lower()
        
        if command in ['setup', '--setup']:
            # Run setup
            script_dir = Path(__file__).parent
            new_cicd_script = script_dir / "luciancicd_v2.py"
            subprocess.run([sys.executable, str(new_cicd_script), "--setup"])
            
        elif command in ['scan', '--scan-only']:
            # Run scan only
            script_dir = Path(__file__).parent
            new_cicd_script = script_dir / "luciancicd_v2.py"
            subprocess.run([sys.executable, str(new_cicd_script), "--scan-only"])
            
        else:
            # Default to running CI/CD
            run_legacy_command()
    else:
        # Default behavior
        run_legacy_command()


if __name__ == "__main__":
    main()