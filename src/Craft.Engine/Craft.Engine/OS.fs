module Craft.Engine.OS

open System
open System.Collections.Generic
open System.Runtime.InteropServices

[<Literal>]
let WindowClassName = "CraftWindow"

[<Struct>]
type RawRectangle = {
    left            : int
    top             : int
    right           : int
    bottom          : int
}

let newRect l t w h = { left = l; top = t; right = l + w; bottom = t + h }

type WM =
    | Destroy = 0x2u
    | Size = 0x5u
    | Paint = 0xFu
    | Close = 0x10u
    | Quit = 0x12u
    | EraseBKGND = 0x14u
    | User = 0x400u
    | GameDone = 0x400u
    | MouseLost = 0x215u
    | MouseMove = 0x200u
    | MouseLeftDown = 0x201u
    | MouseLeftUp = 0x202u
    | KeyDown = 0x0100u
    | KeyUp = 0x0101u

type WindowsDesc = {
    mutable windowedRect            : RawRectangle
    mutable fullscreenRect          : RawRectangle
    mutable clientRect              : RawRectangle
    bigIcon                         : IntPtr
    smallIcon                       : IntPtr
    windowFlags                     : uint32
    mutable fullScreen              : bool
    mutable maximized               : bool
    mutable minimized               : bool
    mutable hide                    : bool
    mutable noResizeFrame           : bool
    mutable borderLessWindow        : bool
    mutable hwnd                    : IntPtr
    mutable resize                  : (int*int) option
}

type CS =
    | HReDraw = 0x2u
    | VReDraw = 0x1u
    | OWNDC = 0x20u
    | All = 0x23u
            
type SystemMetric =
    | CXScreen = 0
    | CYScreen = 1
   
[<Struct>]
[<StructLayout(LayoutKind.Sequential, CharSet = CharSet.Unicode)>]
type WNDCLASSEX = {
    cbSize        : uint32
    style         : uint32
    wndProc       : IntPtr
    cbClsExtra    : int
    cbWndExtra    : int
    hInstance     : IntPtr
    hIcon         : IntPtr
    hCursor       : IntPtr
    hBackground   : IntPtr
    [<MarshalAs(UnmanagedType.LPWStr)>]
    menuName      : string
    [<MarshalAs(UnmanagedType.LPWStr)>]
    className     : string
    hIconSm       : IntPtr
}

[<Struct>]
[<StructLayout(LayoutKind.Sequential)>]
type Message = {
    hwnd                : IntPtr
    mutable message     : WM
    mutable wParam      : IntPtr
    mutable lParam      : IntPtr
    mutable time        : uint32
    mutable x           : int64
    mutable y           : int64
    mutable priv        : int32
}

[<Flags>]
type WindowStyles =
    | Border = 0
    | Overlapped = 0
    | Caption = 0x00c00000
    | SysMenu = 0x00080000
    | ThickFrame = 0x00040000
    | MinimizedBox = 0x00010000
    | MaximizedBox = 0x00020000
    | Visible = 0x10000000
    | Popup = 0x80000000
    | ClipChildren = 0x02000000
    | ClipSiblings = 0x04000000
    
let OverlappedWindow = WindowStyles.Overlapped ||| WindowStyles.Caption ||| WindowStyles.SysMenu ||| WindowStyles.ThickFrame ||| WindowStyles.MinimizedBox ||| WindowStyles.MaximizedBox

type ShowWindowCommand =
    | Hide = 0
    | Normal = 1
    | Minimized = 2
    | Maximize = 3
    
type WindowExStyles =
    | None = 0x0
    | ToolWindow = 0x80

type Input =
    | MouseLeft of bool
    | MouseMove of int * int
    | KeyDown of int
    | KeyUp of int
    
let inputQueue = new Queue<_>()    

[<DllImport("kernel32")>]
extern IntPtr GetModuleHandleW(string name)

[<DllImport("user32.dll", SetLastError=true)>]
extern bool SetProcessDPIAware()

[<DllImport("user32.dll", SetLastError = true,  EntryPoint = "CreateWindowExW")>]
extern IntPtr CreateWindowExW (uint32 exStyles, IntPtr lpClassName, [<MarshalAs(UnmanagedType.LPWStr)>]string lpWindowName, WindowStyles dwStyle, int x, int y, int w, int h, IntPtr parent, IntPtr menu, IntPtr instance, IntPtr lParam)

[<DllImport("user32", EntryPoint = "RegisterClassExW")>]
extern uint16 RegisterClassExW(WNDCLASSEX& cls)

[<DllImport("user32")>]
extern IntPtr LoadIconW (IntPtr instance, IntPtr iconName)

[<DllImport("user32")>]
extern IntPtr LoadCursorW (IntPtr instance, IntPtr iconName)

[<DllImport("user32", ExactSpelling = true)>]
extern bool ShowWindow(IntPtr hWnd, ShowWindowCommand nCmdShow);

[<DllImport("user32")>]
extern void PostQuitMessage(int exitCode)

[<DllImport("user32.dll")>]
extern int GetSystemMetrics(SystemMetric smIndex)        
    
[<DllImport("user32", CharSet = CharSet.Unicode)>]
extern bool AdjustWindowRect (RawRectangle& rect, WindowStyles dwStyle, bool hasMenu)

[<DllImport("user32")>]
extern bool GetWindowRect (IntPtr hwnd, RawRectangle& rect)

[<DllImport("user32")>]
extern bool GetClientRect (IntPtr hwnd, RawRectangle& rect)

[<DllImport("user32.dll", SetLastError = true)>]
extern uint32 SetWindowLongPtr(IntPtr hWnd, int nIndex, WindowStyles value);

[<UnmanagedFunctionPointer(CallingConvention.Winapi)>]
type WINDPROC =
    delegate of
        a: IntPtr *
        [<In; MarshalAs(UnmanagedType.I4)>] b:WM *
        c: IntPtr *
        d: IntPtr -> IntPtr

[<DllImport("user32")>]
extern IntPtr DefWindowProcW(IntPtr hwnd, int msg, IntPtr wparam, IntPtr lparam)

[<DllImport("user32.dll")>]
extern int FillRect(IntPtr hDC, RawRectangle lprc, IntPtr hbr)

[<DllImport("gdi32.dll")>]
extern IntPtr CreateSolidBrush(uint32 crColor);

let adjustWindow (desc: WindowsDesc) =
    if desc.fullScreen then
        GetWindowRect (desc.hwnd, &desc.windowedRect) |> ignore
        SetWindowLongPtr (desc.hwnd, -16, WindowStyles.SysMenu ||| WindowStyles.Popup ||| WindowStyles.ClipChildren ||| WindowStyles.ClipSiblings ||| WindowStyles.Visible) |> ignore
        ShowWindow(desc.hwnd, ShowWindowCommand.Maximize) |> ignore
    else
        if desc.maximized then ShowWindow (desc.hwnd, ShowWindowCommand.Maximize) |> ignore
        else ShowWindow (desc.hwnd, ShowWindowCommand.Normal) |> ignore
        
let mutable windowDesc = Unchecked.defaultof<WindowsDesc>

let getRectWidth (r: RawRectangle) = r.right - r.left    
let getRectHeight (r: RawRectangle) = r.bottom - r.top


let wndProc (hwnd: IntPtr) (msg: WM) (wP: IntPtr) (l: IntPtr) =
    match msg with
    | WM.EraseBKGND ->
        let mutable rect = Unchecked.defaultof<_>
        if GetClientRect(hwnd, &rect) then
            let white = CreateSolidBrush(0x00000000u)
            FillRect (wP, rect, white) |> ignore
        IntPtr.Zero
    | WM.Size ->
        if wP = IntPtr 1 then 
            windowDesc.minimized <- true
        else            
            GetClientRect(hwnd, &windowDesc.clientRect) |> ignore
            let w = windowDesc.clientRect |> getRectWidth
            let h = windowDesc.clientRect |> getRectHeight
            windowDesc.resize <- Some (w, h) 
        IntPtr.Zero
    | WM.GameDone ->
        IntPtr.Zero
    | WM.Close             
    | WM.Destroy ->
        PostQuitMessage(0)       
        IntPtr.Zero
    | WM.MouseMove ->
        let t = l.ToInt64() |> uint32
        let x = int16 t
        let y = int16 (t >>> 16)
        MouseMove (int x, int y) |> inputQueue.Enqueue
        IntPtr.Zero
    | WM.MouseLeftUp ->
        MouseLeft false |> inputQueue.Enqueue
        IntPtr.Zero
    | WM.MouseLeftDown -> 
        MouseLeft true |> inputQueue.Enqueue
        IntPtr.Zero
    | WM.KeyDown ->
        KeyDown (wP.ToInt32()) |> inputQueue.Enqueue
        IntPtr.Zero
    | WM.KeyUp ->
        KeyUp (wP.ToInt32()) |> inputQueue.Enqueue
        IntPtr.Zero
    | _ ->
        DefWindowProcW(hwnd, int msg, wP, l)
        
let wnd = WINDPROC(wndProc)
        
let registerWindowClass() =
    let instance = GetModuleHandleW null
    let icon = LoadIconW (IntPtr.Zero, IntPtr(32512))        
    let cursor = LoadCursorW (IntPtr.Zero, IntPtr(32512))        
    let mutable cls : WNDCLASSEX = {
            cbSize = Marshal.SizeOf<WNDCLASSEX>() |> uint32
            style = 0x23u //User32.CS.All
            hInstance = instance
            hIcon = icon
            hIconSm = icon
            hCursor = cursor
            hBackground = IntPtr.Zero
            wndProc =
                Marshal.GetFunctionPointerForDelegate(wnd)
                //Marshal.GetFunctionPointerForDelegate(wnd.GetInvocationList().[0])
            className = WindowClassName
            cbClsExtra = 0
            cbWndExtra = 0
            menuName = null
        }
    let atom = RegisterClassExW(&cls)                
    if atom = 0us then failwith "Failed to register window class"        
    atom
    
[<DllImport("user32.dll")>]    
extern bool PeekMessageW(Message& lpMsg, IntPtr hWnd, uint32 wMsgFilterMin, uint32 wMsgFilterMax, uint32 wRemoveMsg)

[<DllImport("user32", ExactSpelling = true)>]
extern bool TranslateMessage(Message lpMsg);

[<DllImport("user32", CharSet = CharSet.Unicode)>]
extern IntPtr DispatchMessage(Message lpmsg);    

let handleMessages () =
    let mutable msg = Unchecked.defaultof<Message>
    let mutable q = false
    while PeekMessageW(&msg, IntPtr.Zero, 0u, 0u, 1u) do    
        TranslateMessage (msg) |> ignore
        DispatchMessage msg |> ignore
        if msg.message = WM.Quit || msg.message = WM.Close then
            q <- true
    q
        
let openWindow (name: string) (desc: WindowsDesc) =        
    windowDesc <- desc
    desc.fullscreenRect <- newRect 0 0 (GetSystemMetrics SystemMetric.CXScreen) (GetSystemMetrics SystemMetric.CYScreen)
    if desc.clientRect |> getRectWidth <= 0 || desc.clientRect |> getRectHeight <= 0 then
        let w = min 1920 (GetSystemMetrics SystemMetric.CXScreen)
        let h = min 1080 (GetSystemMetrics SystemMetric.CYScreen)
        desc.clientRect <- newRect 0 0 w h 
    let mutable windowStyle = OverlappedWindow
    if desc.noResizeFrame then windowStyle <- windowStyle ^^^ (WindowStyles.ThickFrame ||| WindowStyles.MaximizedBox)
    if desc.borderLessWindow then windowStyle <- windowStyle ^^^ WindowStyles.Caption
    
    AdjustWindowRect (&desc.clientRect, windowStyle, false) |> ignore        
    let rect = desc.clientRect
//
    let atom = registerWindowClass()
    let hwnd =
        CreateWindowExW (
            0u,
            IntPtr(int atom),
            name,
            windowStyle ||| ((if desc.hide then WindowStyles.Overlapped else WindowStyles.Visible) ||| WindowStyles.Border),
            //Common.WS.OVERLAPPEDWINDOW,
            (if rect.left < 0 then 0x80000000 else rect.left),
            (if rect.top < 0 then 0x80000000 else rect.top),
            rect |> getRectWidth,
            rect |> getRectHeight,
            IntPtr.Zero,
            IntPtr.Zero,
            GetModuleHandleW null,
            IntPtr.Zero
        )
 
    if hwnd = IntPtr.Zero then failwithf "Can't create window"
    
    desc.hwnd <- hwnd
    
    if not desc.hide then         
        if desc.maximized then ShowWindow (hwnd, ShowWindowCommand.Maximize) |> ignore
        elif desc.maximized then ShowWindow (hwnd, ShowWindowCommand.Minimized) |> ignore
        elif desc.fullScreen then adjustWindow (desc) |> ignore
        else
            ShowWindow (hwnd, ShowWindowCommand.Normal) |> ignore           
    ()            
