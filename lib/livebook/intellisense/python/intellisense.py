import ast
import importlib
import importlib.util
import inspect
import pkgutil
import sys
import types
from typing import Callable


def variable_matches(hint, matching_python_vars, matcher):
    infos = {}

    for name, object in matching_python_vars.items():
        infos[name] = get_info(name, object)

    for name, object in globals()["__builtins__"].items():
        if not matches(matcher, name, hint) or (
            name.startswith("_") and not name.startswith("__")
        ):
            continue

        infos[name] = get_info(name, object)

    return infos


def dot_matches(target_object, target_part, parts, matcher):
    if target_object is None:
        for name, object in globals()["__builtins__"].items():
            if name == target_part:
                target_object = object
                break

    if target_object is None:
        return []

    hint = parts[-1]

    for part in parts[0:-1]:
        if not hasattr(target_object, part):
            return []
        target_object = getattr(target_object, part)

    infos = []

    for attr in dir(target_object):
        if not matches(matcher, attr, hint) or (
            attr.startswith("_") and not attr.startswith("__")
        ):
            continue

        value = getattr(target_object, attr)
        infos.append(get_info(attr, value))

    return infos


def import_matches(hint, matcher):
    # We intentionally don't import modules here to avoid side effects.
    # We just list available module names without loading them. We try
    # to extract docstrings from source when available.

    infos = []

    if "." in hint:
        # Handle submodules like "numpy.ra" -> "numpy.random".
        parent_module, submodule_hint = hint.rsplit(".", 1)

        try:
            spec = importlib.util.find_spec(parent_module)
            if spec and spec.submodule_search_locations:
                for importer, modname, ispkg in pkgutil.iter_modules(
                    spec.submodule_search_locations
                ):
                    if matches(
                        matcher, modname, submodule_hint
                    ) and not modname.startswith("_"):
                        full_name = f"{parent_module}.{modname}"
                        doc = get_module_docstring_from_source(full_name)
                        infos.append(
                            {
                                "kind": "module",
                                "name": full_name,
                                "intrinsic_name": full_name,
                                "documentation": doc,
                            }
                        )
        except Exception:
            pass
    else:
        # Handle top-level modules like "nu" -> "numpy".
        for importer, modname, ispkg in pkgutil.iter_modules():
            if matches(matcher, modname, hint) and not modname.startswith("_"):
                doc = get_module_docstring_from_source(modname)
                infos.append(
                    {
                        "kind": "module",
                        "name": modname,
                        "intrinsic_name": modname,
                        "documentation": doc,
                    }
                )

        # Also include built-in modules (like sys, time, builtins).
        for modname in sys.builtin_module_names:
            if matches(matcher, modname, hint) and not modname.startswith("_"):
                infos.append(
                    {
                        "kind": "module",
                        "name": modname,
                        "intrinsic_name": modname,
                        "documentation": None,
                    }
                )

    return infos


def import_member_matches(module_name, hint, matcher):
    # In this case we load the module to complete importable names.
    # Importing this single module should be fine, since at this point
    # we know the user intends to import it anyway.

    infos = []

    try:
        mod = importlib.import_module(module_name)
        for attr in dir(mod):
            if matches(matcher, attr, hint) and not attr.startswith("_"):
                value = getattr(mod, attr)
                infos.append(get_info(attr, value))
    except Exception:
        pass

    return infos


def get_info(name, object):
    if inspect.ismodule(object):
        return {
            "kind": "module",
            "name": name,
            "intrinsic_name": object.__name__,
            "documentation": get_documentation(object),
        }
    elif inspect.isclass(object):
        return {
            "kind": "class",
            "name": name,
            "intrinsic_name": object.__name__,
            "documentation": get_documentation(object),
            "signature": get_signature(object),
            "documentation_signature": get_signature_from_doc(object),
            "has_parameters": has_parameters(object),
        }
    elif inspect.ismethod(object):
        return {
            "kind": "method",
            "name": name,
            "intrinsic_name": object.__name__,
            "documentation": get_documentation(object),
            "signature": get_signature(object),
            "documentation_signature": get_signature_from_doc(object),
            "has_parameters": has_parameters(object),
        }
    elif (
        inspect.isfunction(object)
        or inspect.isbuiltin(object)
        # NumPy functions are not regular functions, but they are callable,
        # so we want to treat those as functions.
        or isinstance(object, Callable)
    ):
        return {
            "kind": "function",
            "name": name,
            "intrinsic_name": object.__name__,
            "documentation": get_documentation(object),
            "signature": get_signature(object),
            "documentation_signature": get_signature_from_doc(object),
            "has_parameters": has_parameters(object),
        }
    else:
        return {"kind": "variable", "name": name}


def get_documentation(object):
    doc = inspect.getdoc(object)

    if doc is None:
        return None

    # If there is a signature in the docs (numpy-style), we remove it.
    if doc.startswith(object.__name__ + "("):
        pos = doc.find("\n\n")
        if pos == -1:
            return None
        else:
            return doc[(pos + 2) :]

    return doc


module_docstring_cache = {}


# Gets module documentation without importing the module. Returns
# None if there is no source information.
def get_module_docstring_from_source(module_name):
    if module_name in module_docstring_cache:
        return module_docstring_cache[module_name]

    try:
        spec = importlib.util.find_spec(module_name)
        if spec and spec.origin and spec.origin.endswith(".py"):
            with open(spec.origin, "r") as f:
                tree = ast.parse(f.read())
            docstring = ast.get_docstring(tree)
            module_docstring_cache[module_name] = docstring
            return docstring
    except Exception:
        pass

    return None


def get_signature_from_doc(object):
    # If the function has numpy-style docs with a readability-optimised
    # signature in the first line, we extract it.
    doc = inspect.getdoc(object)

    if doc is None:
        return None

    if doc.startswith(object.__name__ + "("):
        pos = doc.find("\n\n")
        if pos != -1:
            doc = doc[:pos]
        return get_module_prefix(object) + doc

    return None


def get_signature(object):
    try:
        signature = inspect.signature(object)
        return get_module_prefix(object) + object.__name__ + signature.format()
    except Exception:
        # Certain native functions don't have siganture, in which case
        # inspect.signature throws.
        return None


def get_module_prefix(object):
    try:
        if object.__module__ in ["builtins", "__main__"]:
            return ""
        elif object.__module__ is not None:
            return object.__module__ + "."
        else:
            return object.__qualname__[: -len(object.__name__)]
    except Exception:
        return ""


def has_parameters(object):
    try:
        signature = inspect.signature(object)
        return len(signature.parameters) > 0
    except Exception:
        return None


def matches(matcher, string, hint):
    if matcher == "exact":
        return string == hint
    if matcher == "prefix":
        return string.startswith(hint)

    raise ValueError(f"Unknown matcher: {matcher}")


# Build intellisense module, exposing functions that we call in
# Livebook.Intellisense.Python.

intellisense = types.ModuleType("intellisense")
intellisense.variable_matches = variable_matches
intellisense.dot_matches = dot_matches
intellisense.import_matches = import_matches
intellisense.import_member_matches = import_member_matches

intellisense
