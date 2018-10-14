package main

/*
A simple utility command to show how to use loader package in golang
and parse Golang code in Golang.

This utility mainly has code for
- finding implementations of a interface
- listing functions of a interface
- parsing function params and results
- adding proper imports used my implementations
*/
import (
	"flag"
	"fmt"
	"go/types"
	"golang.org/x/tools/go/loader"
	"os"
	"path"
	"strconv"
	"strings"
)

var imports = make(map[string]string)

func main() {
	path := flag.String("path", ".", "Directory path to use")
	ifaces := flag.Bool("list-interfaces", false, "Lists all interfaces available in given path")
	types := flag.Bool("types", false, "Lists all type available in given path")
	iface := flag.String("interface", "", "Name of Golang interface")
	impls := flag.Bool("implementations", false, "Return implementations of interface name provided in --interfaces")

	flag.Parse()

	// if list-interfaces flag provided, lets get the interfaces
	if *ifaces {
		if path == nil || len(*path) == 0 {
			fmt.Printf("ERROR: When asking for interfacess, --path must be provided")
			os.Exit(1)
		}
		fmt.Printf("Interfaces under path '%s'\n", *path)
		// get types from a directory path provided
		typs := getTypes(*path)
		// Let's find interfaces from the types we found in given path
		for _, i := range getInterfaces(typs) {
			fmt.Printf("\t- %s\n", i.Name)
		}
		return
	}

	// if implementations flag is provided, lets get the implementations of a interface provided
	if *impls {
		if iface == nil || len(*iface) == 0 {
			fmt.Printf("ERROR: When asking for implementations, --interface must be provided")
			os.Exit(1)
		}
		fmt.Println(*iface)

		typs := getTypes(*path)

		// We need the types to find the implementations of a interface
		impls, funcs := getImpls(typs, *iface)
		for k := range impls {
			fmt.Printf("\t- %s\n", k)
		}

		for _, im := range impls {
			imports[im] = normalizeImportName(im)
		}

		fmt.Println("Functions on the interface")
		for _, f := range funcs {
			fmt.Printf("\t- %s%s %+v\n", f.name, f.params, f.results)
		}

		// Let's get how to generate a function call. What we did above is the function definition itseld
		fmt.Println("Calle funcs")
		for _, f := range getFuncsAsCaller(funcs) {
			// print function name and args
			if len(f.callerArgs) == 0 {
				fmt.Printf("\t- %s()", f.Name)
			} else {
				fmt.Printf("\t- %s(", f.Name)
			}

			for idx, arg := range f.callerArgs {
				if idx != len(f.callerArgs)-1 {
					fmt.Printf("%s,", arg.name)
				} else {
					fmt.Printf("%s)", arg.name)
				}
			}

			// print return types if available
			if len(f.returnArgs) == 0 {
				fmt.Println()
			}
			for idx, arg := range f.returnArgs {
				fmt.Printf(" (")
				if idx != len(f.returnArgs)-1 {
					fmt.Printf("%s,", arg.name)
				} else {
					fmt.Printf("%s)", arg.name)
				}
			}

			// print default returns
			if len(f.DefaultReturnArgs) == 0 {
				fmt.Println()
			} else {
				fmt.Printf(" {\n\t\treturn ")
			}

			for idx, ret := range f.DefaultReturnArgs {
				if idx != len(f.DefaultReturnArgs)-1 {
					fmt.Printf("%s,", ret)
				} else {
					fmt.Printf("%s\n\t}\n", ret)
				}
			}
		}

		return
	}

	// Just dump all types when types flag is provided
	if *types {
		fmt.Printf("Types available under path '%s'\n", *path)
		for _, t := range getTypes(*path) {
			fmt.Printf("\t- %s\n", t.TypeName.Name())
		}
		return
	}
}

// getTypes will return all types available under given path
func getTypes(path string) []typeMeta {
	// init the loader config
	conf := &loader.Config{}
	// import the path to config
	conf.Import(path)

	// load the config, which loads all the code under given path into loader config
	program, err := conf.Load()
	if err != nil {
		fmt.Printf("ERROR: %s\n", err.Error())
		return nil
	}

	// lets define a slice to capture some metadata about each type we find
	// We will use some of this information for doing other things later on
	var typs []typeMeta

	// IntialPackages will return all packages we got under given path
	for _, pkg := range program.InitialPackages() {
		// scope will return go code scope for each package
		scope := pkg.Pkg.Scope()
		// loop over the scope names, which are actual object names
		for _, n := range scope.Names() {
			// lets lookup the objects in the scope based on their name
			obj := scope.Lookup(n)
			// We only care about types, not variables/constants, so lets type check and skip rest
			if typ, ok := obj.(*types.TypeName); ok {
				typs = append(typs, typeMeta{
					Package:  pkg.Pkg.Name(),
					Object:   obj,
					TypeName: typ,
					Pointer:  types.NewPointer(typ.Type()),
				})
			}
		}

		// From package object, we can also parse code file by file
		// What we do here is to grab the imports the files have, we need
		// track this to be able to generate code with all dependent imports
		// in place
		for _, f := range pkg.Files {
			for _, i := range f.Imports {
				addImport(i.Path.Value)
			}
		}
	}

	return typs
}

// addImport will add an import to a map for future reference
func addImport(importValue string) {
	importValue, _ = strconv.Unquote(importValue)
	base := path.Base(importValue)
	if strings.Contains(base, ".") {
		base = strings.Replace(base, ".", "", -1)
	}
	imports[importValue] = normalizeImportName(base)
}

// notmalizeImportName will normalize the import path to be compatible with Go code
func normalizeImportName(s string) string {
	s = strings.Replace(s, "/", "_", -1)
	s = strings.Replace(s, ".", "_", -1)
	s = strings.Replace(s, "-", "", -1)
	return s
}

// getInterfaces will return the slice of interfaces from the given types
func getInterfaces(typs []typeMeta) (interfaces []interfaceMeta) {
	for _, typ := range typs {
		if iface, ok := typ.TypeName.Type().Underlying().(*types.Interface); ok { // Only interfaces
			interfaces = append(interfaces, interfaceMeta{typ.Object.Name(), iface, typ.Object})
		}
	}

	return
}

// getImpls will return all implementation of a interfaceName from given allTypes
func getImpls(allTypes []typeMeta, interfaceName string) (map[string]string, []interfaceFuncs) {
	interfaces := getInterfaces(allTypes)
	var interfaceFunctions []interfaceFuncs

	processed := make(map[string]struct{})
	processedPointers := make(map[string]struct{})
	implementedBy := make(map[string]string)
	// We might have more tha 1 interface in the types, so lets loop over all of them
	for _, iface := range interfaces {
		// Let's go ahead only if interface name is same as what we are looking for
		if iface.Name != interfaceName {
			continue
		}

		// Let's get all methods we have on this interface, so we can list all methods if needed
		for i := 0; i < iface.Underlying.NumMethods(); i++ {
			iFuncs := interfaceFuncs{}
			meth := iface.Underlying.Method(i)
			iFuncs.name = meth.Name()
			sign := meth.Type().Underlying().(*types.Signature)
			iFuncs.params = sign.Params()
			iFuncs.results = sign.Results()
			iFuncs.variadic = sign.Variadic()
			interfaceFunctions = append(interfaceFunctions, iFuncs)
		}

		// this is importtant!!!
		if iface.Underlying.NumMethods() == 0 {
			// Everything implements empty interfaces, skip those
			continue
		}

		// Now that we have our interface object, lets go over all the types and find its implementations
		for _, typ := range allTypes {
			// An interface will always implement itself, so skip those
			if typ.Object.Pkg() == iface.Obj.Pkg() && typ.Object.Name() == iface.Name {
				continue
			}

			// we check if current type is assignanle to a interface, if we this type implements a interface
			if types.AssignableTo(typ.Object.Type(), iface.Underlying) {
				implementedBy[typ.Object.Name()] = typ.TypeName.Pkg().Path()
				processed[typ.Object.Name()] = struct{}{}
			}

			// Same as above, but for a pointer type implementations
			if _, ok := typ.TypeName.Type().Underlying().(*types.Interface); !ok {
				if types.AssignableTo(typ.Pointer.Underlying(), iface.Underlying) {
					if _, ok := processed[typ.Object.Name()]; !ok {
						implementedBy[typ.Object.Name()] = typ.TypeName.Pkg().Path()
						processedPointers[typ.Object.Name()] = struct{}{}
					}
				}
			}
		}
	}

	return implementedBy, interfaceFunctions
}

// getFuncsAsCaller will return function as a caller
func getFuncsAsCaller(interfaceFunctions []interfaceFuncs) []functionDetails {
	var funcs []functionDetails
	for _, ifunc := range interfaceFunctions {
		// process function call arguments
		var params []functionParam     // Keep track of arguments to the function in definition
		var callParams []functionParam // Keep track of arguments to the function when calling it, they will have to be without the type definitions
		for i := 0; i < ifunc.params.Len(); i++ {
			p := ifunc.params.At(i)
			customType, _ := checkAndAddImport(p.Type())
			if customType {
				if i == ifunc.params.Len()-1 && ifunc.variadic {
					params = append(params, functionParam{name: getFunctionName(p.Name(), p.Type(), true), typ: p.Type()})
				} else {
					params = append(params, functionParam{name: getFunctionName(p.Name(), p.Type(), false), typ: p.Type()})
				}
			} else {
				if i == ifunc.params.Len()-1 && ifunc.variadic {
					parenthesisRemovedType := p.Type().String()[2:]
					params = append(params, functionParam{name: fmt.Sprintf("%s %s", p.Name(), fmt.Sprintf("...%s", parenthesisRemovedType)), typ: p.Type()})
				} else {
					params = append(params, functionParam{name: fmt.Sprintf("%s %s", p.Name(), p.Type().String()), typ: p.Type()})
				}
			}
			if i == ifunc.params.Len()-1 && ifunc.variadic {
				callParams = append(callParams, functionParam{name: fmt.Sprintf("%s...", p.Name())})
			} else {
				callParams = append(callParams, functionParam{name: fmt.Sprintf("%s", p.Name())})
			}
		}
		f := functionDetails{
			Name:       ifunc.name,
			args:       params,
			callerArgs: callParams,
		}

		// process function return arguments
		var results []functionParam // Keep track of the return statement on function definition
		var defaultReturn []string  // Keep track of zero value returns we have to do in case on not matching any ES versions on read/write calls
		for i := 0; i < ifunc.results.Len(); i++ {
			r := ifunc.results.At(i)
			customType, isErrorType := checkAndAddImport(r.Type())
			if customType {
				results = append(results, functionParam{name: getFunctionName(r.Name(), r.Type(), false), typ: r.Type()})
			} else {
				if len(r.Name()) > 0 {
					results = append(results, functionParam{name: fmt.Sprintf("%s %s", r.Name(), r.Type().String()), typ: r.Type()})
				} else {
					results = append(results, functionParam{name: fmt.Sprintf("%s", r.Type().String()), typ: r.Type()})
				}
			}
			if i == ifunc.params.Len()-1 && isErrorType {
				f.hasErrorReturn = true
			}
			defaultReturn = append(defaultReturn, getZeroValueString(r.Type()))
		}
		if len(results) > 0 {
			f.returnArgs = results
			f.DefaultReturnArgs = defaultReturn
		}

		funcs = append(funcs, f)
	}

	return funcs
}

// getZeroValueString will return the zero value of a type for both standard lib types and named/custom types
func getZeroValueString(typ types.Type) string {
	switch typ.(type) {
	case *types.Basic:
		basic := typ.(*types.Basic)
		switch basic.Kind() {
		case types.Bool:
			return "false"
		case types.Int, types.Int8, types.Int16, types.Int32, types.Int64:
			return "-1"
		case types.Uint, types.Uint8, types.Uint16, types.Uint32, types.Uint64:
			return "0"
		case types.Float32, types.Float64:
			return "0.0"
		case types.String:
			return `""`
		}
	case *types.Array, *types.Slice, *types.Chan, *types.Interface, *types.Map, *types.Pointer:
		return "nil"
	case *types.Named:
		if typ.(*types.Named).String() == "error" {
			return `errors.New("Smart Client: Unable to match any available ES repo to delegate to")`
		}
		_, importType, name := splitTypeAndName(typ.(*types.Named))
		return fmt.Sprintf("%s.%s{}", importType, name)
	}
	return ""
}

func getFunctionName(name string, typ types.Type, variadic bool) string {
	return fmt.Sprintf("%s %s", name, getImportedType(typ, "", variadic))
}

// getImportedType will return a import string, based on type provided
func getImportedType(typ types.Type, prefix string, variadic bool) string {
	switch typ.(type) {
	case *types.Chan:
		chanDirection := ""
		switch typ.(*types.Chan).Dir() {
		case types.SendRecv:
			chanDirection = ""
		case types.SendOnly:
			chanDirection = "->"
		case types.RecvOnly:
			chanDirection = "<-"
		}
		return getImportedType(typ.(*types.Chan).Elem(), fmt.Sprintf("%schan %s", chanDirection, prefix), variadic)
	case *types.Pointer:
		return getImportedType(typ.(*types.Pointer).Elem(), fmt.Sprintf("%s*", prefix), variadic)
	case *types.Slice:
		if variadic {
			return getImportedType(typ.(*types.Slice).Elem(), fmt.Sprintf("%s...", prefix), variadic)
		}
		return getImportedType(typ.(*types.Slice).Elem(), fmt.Sprintf("%s[]", prefix), variadic)
	default:
		str := typ.String()
		var namedType string
		for i := len(str) - 1; i >= 0; i-- {
			if string(str[i]) == "." {
				namedType = str[i+1:]
				str = str[0:i]
				break
			}
		}

		// This looks a bit weird, but needed for usecase like
		// we have import like context "golang.org/x/context" and params are using it as context.Context
		// when we try to check import for this type we get empty, because the mapping is between
		// "golang.org/x/context" to context, so we end up with empty type like ".Context" instead of "context.Context"
		name := imports[str]
		if imports[str] == "" {
			for _, v := range imports {
				if v == str {
					name = str
					break
				}
			}
		}
		return fmt.Sprintf("%s%s.%s", prefix, name, namedType)
	}
}

// checkAndAddImport will check type of given type and import this type if not belonging to standarad lib
func checkAndAddImport(typ types.Type) (bool, bool) {
	switch typ.(type) {
	case *types.Chan:
		return checkAndAddImport(typ.(*types.Chan).Elem())
	case *types.Slice:
		return checkAndAddImport(typ.(*types.Slice).Elem())
	case *types.Named:
		// hack to not think error named type, even though it is :D
		if typ.(*types.Named).String() == "error" {
			return false, true
		}
		actualType, importType, _ := splitTypeAndName(typ.(*types.Named))
		for _, v := range imports {
			if v == importType {
				return true, false
			}
		}
		imports[actualType] = importType
		return true, false
	case *types.Pointer:
		return checkAndAddImport(typ.(*types.Pointer).Elem())
	}
	return false, false
}

// splitTypeAndName will split the name and type of the type, so we can easily add these to imports
func splitTypeAndName(typ types.Type) (string, string, string) {
	str := typ.String()
	var namedType string
	for i := len(str) - 1; i >= 0; i-- {
		if string(str[i]) == "." {
			namedType = str[i+1:]
			str = str[0:i]
			break
		}
	}
	if _, ok := imports[str]; !ok {
		return str, str, namedType
	}
	return str, imports[str], namedType
}

// interfaceMeta defines the metadata about a interface, like name, interface object etc
type interfaceMeta struct {
	Name       string
	Underlying *types.Interface
	Obj        types.Object
}

// typeMeta defines metadata of a type, like name, package it belongs to etc
type typeMeta struct {
	Package  string
	Object   types.Object
	TypeName *types.TypeName
	Pointer  *types.Pointer
}

// interfaceFuncs defines name, params, return types of a fucntion
type interfaceFuncs struct {
	name     string
	params   *types.Tuple
	results  *types.Tuple
	variadic bool
}

// functionParam deifnes the name and type of a function parameter
type functionParam struct {
	name string
	typ  types.Type
}

// functionDetails defines metadata needed to build a function call
type functionDetails struct {
	Name              string
	args              []functionParam
	callerArgs        []functionParam
	returnArgs        []functionParam
	hasErrorReturn    bool
	DefaultReturnArgs []string
}
