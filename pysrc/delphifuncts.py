have_delphi_train = False
have_delphi_style = False
have_delphi_io = False

try:
    import pinout

    have_delphi_io = True

    class TDelphiInputOutput:
        def __getattr__(Self, Key):
            return pinout.GetProperty(Key)

        def __setattr__(Self, Key, Value):
            pinout.SetProperty(Key, Value)

        def __repr__(Self):
            tmp = ""
            for i in pinout.GetPropertyList():
                if tmp:
                    tmp = tmp + ", "
                tmp = tmp + i + " = " + str(getattr(Self,i))
            return tmp

except Exception as e:
    print("Missing pinout")
    

try:
    import pstylize

    have_delphi_style = True

    class TDelphiStylize:
        def __getattr__(Self, Key):
            return pstylize.GetProperty(Key)

        def __setattr__(Self, Key, Value):
            pstylize.SetProperty(Key, Value)

        def __repr__(Self):
            tmp = ""
            for i in pstylize.GetPropertyList():
                if tmp:
                    tmp = tmp + ", "
                tmp = tmp + i + " = " + str(getattr(Self,i))
            return tmp

except Exception as e:
    print("Missing pstylize")
    
try:
    import ptrain

    have_delphi_train = True

    class TDelphiTrain:
        def __getattr__(Self, Key):
            return ptrain.GetProperty(Key)

        def __setattr__(Self, Key, Value):
            ptrain.SetProperty(Key, Value)

        def __repr__(Self):
            tmp = ""
            for i in ptrain.GetPropertyList():
                if tmp:
                    tmp = tmp + ", "
                tmp = tmp + i + " = " + str(getattr(Self,i))
            return tmp

except Exception as e:
    print("Missing ptrain")
  
