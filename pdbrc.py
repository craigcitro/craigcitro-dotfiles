# import pdb, rlcompleter, sys

def complete(self, text, state):
    if not hasattr(self, 'completer'):
        self.completer = _rlcompleter.Completer(self.curframe.f_locals)
    else:
        self.completer.namespace = self.curframe.f_locals
    return self.completer.complete(text, state)

# if (len(sys._current_frames()) > 1):
#   frame = sys._getframe(1)
#   if (frame.f_locals.has_key('self') and
#       isinstance(frame.f_locals['self'], pdb.Pdb)):
#     frame.f_locals['self'].use_rawinput = 1
#     frame.f_globals['Pdb'].complete = complete

#del complete, pdb, rlcompleter, sys
