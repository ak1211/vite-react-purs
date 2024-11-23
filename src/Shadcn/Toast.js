import { Toaster } from "@/components/ui/toaster"
import { useToast } from "@/hooks/use-toast"
export const toasterImpl = Toaster
export const useToastImpl = () => { const { toast } = useToast(); return toast; }