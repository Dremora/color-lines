Elm.Native.ColorLines = {};
Elm.Native.ColorLines.make = function(localRuntime) {

	localRuntime.Native = localRuntime.Native || {};
	localRuntime.Native.ColorLines = localRuntime.Native.ColorLines || {};
	if (localRuntime.Native.ColorLines.values)
	{
		return localRuntime.Native.ColorLines.values;
	}
	return localRuntime.Native.ColorLines.values = {
		initialSeed: Math.random() * Math.pow(2,32)
	};
};
