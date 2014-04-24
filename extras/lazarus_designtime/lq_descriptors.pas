unit lq_descriptors;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LazIDEIntf, ProjectIntf, Controls, Forms,
  lq_form;
type
  { TProjectApplicationDescriptor }

  TlqApplicationDescriptor = class(TProjectDescriptor)
  public
    constructor Create; override;
    function GetLocalizedName: string; override;
    function GetLocalizedDescription: string; override;
    function InitProject(AProject: TLazProject): TModalResult; override;
    function CreateStartFiles({%H-}AProject: TLazProject): TModalResult; override;
  end;

{ TFileDescPascalUnitWithPgfForm }

  TFileDescPascalUnitWithLpForm = class(TFileDescPascalUnitWithResource)
  public
    constructor Create; override;
    function GetInterfaceUsesSection: string; override;
    function GetLocalizedName: string; override;
    function GetLocalizedDescription: string; override;
    function GetUnitDirectives: string; override;
    //function GetImplementationSource(const Filename, SourceName,
       //                              ResourceName: string): string; override;
  end;


procedure Register;

implementation
uses lq_designer;

procedure Register;
begin
  //FileDescPascalUnitWithPgfForm := TFileDescPascalUnitWithPgfForm.Create();
  RegisterProjectFileDescriptor(TFileDescPascalUnitWithLpForm.Create,
                                FileDescGroupName);
  RegisterProjectDescriptor(TlqApplicationDescriptor.Create);
end;

function FileDescriptorHDForm() : TProjectFileDescriptor;
begin
  Result:=ProjectFileDescriptors.FindByName('lqForm');
end;

{ TFileDescPascalUnitWithlpForm }

constructor TFileDescPascalUnitWithLpForm.Create;
begin
  inherited Create;
  Name:='lqForm';
  ResourceClass:=TlqForm;
  UseCreateFormStatements:=true;
end;

function TFileDescPascalUnitWithLpForm.GetInterfaceUsesSection: string;
begin
  Result:='Classes, SysUtils, lq_base, lq_main, lq_form';
end;

function TFileDescPascalUnitWithLpForm.GetLocalizedName: string;
begin
  Result:='lqForm';
end;

function TFileDescPascalUnitWithLpForm.GetLocalizedDescription: string;
begin
  Result:='Create a new lqForm for LiteKit Application';
end;

function TFileDescPascalUnitWithLpForm.GetUnitDirectives: string;
begin
  result := inherited GetUnitDirectives();
  result := '{$ifdef fpc}'+ LineEnding
           +result + LineEnding
           +'{$endif}';
end;

{function TFileDescPascalUnitWithlpForm.GetImplementationSource(const Filename,
  SourceName, ResourceName: string): string;
begin
  Result:='{$R *.dfm}'+LineEnding+LineEnding;
end;}

{ TProjectApplicationDescriptor }

constructor TlqApplicationDescriptor.Create;
begin
  inherited;
  Name := 'LiteKit Application';
end;

function TlqApplicationDescriptor.CreateStartFiles(
  AProject: TLazProject): TModalResult;
begin
  Result:=LazarusIDE.DoNewEditorFile(FileDescriptorHDForm,'','',
                         [nfIsPartOfProject,nfOpenInEditor,nfCreateDefaultSrc]);
end;

function TlqApplicationDescriptor.GetLocalizedDescription: string;
begin
  Result := 'LiteKit Application'+LineEnding+LineEnding
           +'An application based on the LiteKit.'+LineEnding
           +'The program files is automatically maintained by Lazarus.';
end;

function TlqApplicationDescriptor.GetLocalizedName: string;
begin
  Result := 'LiteKit Application';
end;

function TlqApplicationDescriptor.InitProject(
  AProject: TLazProject): TModalResult;
var
  NewSource: String;
  MainFile: TLazProjectFile;
begin
  Result:=inherited InitProject(AProject);

  MainFile:=AProject.CreateProjectFile('project1.dpr');
  MainFile.IsPartOfProject:=true;
  AProject.AddFile(MainFile,false);
  AProject.MainFileID:=0;
  AProject.UseAppBundle:=true;
  AProject.UseManifest:=true;
  AProject.LoadDefaultIcon;

  // create program source
  NewSource:='program Project1;'+LineEnding
    +LineEnding
    +'{$ifdef fpc}'+LineEnding
    +'{$mode delphi}{$H+}'+LineEnding
    +'{$endif}'+LineEnding
    +LineEnding
    +'uses'+LineEnding
    //+'  {$IFDEF UNIX}{$IFDEF UseCThreads}'+LineEnding
    //+'  cthreads,'+LineEnding
    //+'  {$ENDIF}{$ENDIF}'+LineEnding
    //+'  Interfaces, // this includes the LCL widgetset'+LineEnding
    +'  lq_base, lq_main, lq_form '+LineEnding
    +'  { you can add units after this };'+LineEnding
    +LineEnding
    +'begin'+LineEnding
    //+'  RequireDerivedFormResource := True;'+LineEnding
    +'  Application.Initialize;'+LineEnding
    +'  Application.Run;'+LineEnding
    +'end.'+LineEnding
    +LineEnding;
  AProject.MainFile.SetSourceText(NewSource,true);

  // add lcl pp/pas dirs to source search path
  AProject.AddPackageDependency('FCL');
  AProject.LazCompilerOptions.Win32GraphicApp:=true;
  AProject.LazCompilerOptions.UnitOutputDirectory:='lib'+PathDelim+'$(TargetCPU)-$(TargetOS)';
  AProject.LazCompilerOptions.TargetFilename:='project1';
end;

end.


